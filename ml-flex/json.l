%top{
#define _DEFAULT_SOURCE

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>

enum {
    T_EOF = 0,
    T_LBRACE,
    T_RBRACE,
    T_LBRACK,
    T_RBRACK,
    T_COMMA,
    T_COLON,
    T_NULL,
    T_FALSE,
    T_TRUE,
    T_STRING,
    T_INT,
    T_FLOAT
};

typedef struct {
    int line;
    int first_column;
    int last_column;
} YYLTYPE;

typedef union {
    char *string_value;
    double float_value;
    int64_t int_value;
} YYSTYPE;

#define YY_USER_ACTION                          \
    yylloc->first_column = yylloc->last_column; \
    yylloc->last_column += yyleng;
}

%{
struct buffer_t {
    char *buf;
    size_t size;
    size_t capacity;
};
static void buffer_init(struct buffer_t *b);
static void buffer_add(struct buffer_t *b, char c);
static void buffer_addu(struct buffer_t *b, uint32_t u);
static void buffer_adds(struct buffer_t *b, char *s, size_t n);
static char *buffer_extract(struct buffer_t *b);
static uint32_t hex4_decode(const char *str);

#if defined(__GNUC__)
#  define unreachable() (__builtin_unreachable())
#elif defined(_MSC_VER)
#  define unreachable() (__assume(false))
#else
#  define unreachable() ((void)0)
#endif
%}

digit  [0-9]
digits {digit}+
int    "-"?([0-9]|[1-9][0-9]+)
frac   "."[0-9]+
exp    [eE][+-]?[0-9]+
xdigit [0-9a-fA-F]

/* Hexcodes for High Surrogates (U+D800-U+DBFF) */
highSurrogateHex   [dD][89abAB]{xdigit}{2}
/* Hexcodes for Low Surrogates (U+DC00-U+DFFF) */
lowSurrogateHex    [dD][c-fC-F]{xdigit}{2}
surrogateHex       {highSurrogateHex}|{lowSurrogateHex}
/* Other hex-codes */
nonSurrogateHex    [0-9a-ce-fA-CE-F]{xdigit}{3}|[dD][0-7]{xdigit}{2}

/* UTF-8 Continuation Byte */
cont          [\x80-\xBF]
/* Non-ASCII UTF-8 Scalar Value */
scalarValue   [\xC0-\xDF]{cont}|[\xE0-\xEC\xEE-\xEF]{cont}{2}|\xED[\x80-\x9f]{cont}|[\xF0-\xF4][\x80-\x8F]{cont}{2}
/* WTF-8 encoded surrogate (0xD800-0xDFFF) */
wtf8Surrogate \xED[\xA0-\xBF][\x80-\xBF]
/* UTF-8 minus control characters, backslash (\), and double quote (") */
stringChar    [\x20-\x21\x23-\x5B\x5D-\x7F]|{scalarValue}

%option noyywrap nounput noinput nodefault
%option reentrant
%option 8bit
%option bison-bridge bison-locations
%option ecs meta-ecs

%x STRING
%x COMMENT

%%

%{
int string_first_column;
struct buffer_t buf;
%}

[\ \t\r]+        ;
\n               yylloc->last_column = 1; yylloc->line += 1;
"{"              return T_LBRACE;
"}"              return T_RBRACE;
"["              return T_LBRACK;
"]"              return T_RBRACK;
","              return T_COMMA;
":"              return T_COLON;
"null"           return T_NULL;
"true"           return T_TRUE;
"false"          return T_FALSE;
{int}            yylval->int_value = strtoll(yytext, NULL, 10); return T_INT;
{int}{frac}      yylval->float_value = strtod(yytext, NULL); return T_FLOAT;
{int}{exp}       yylval->float_value = strtod(yytext, NULL); return T_FLOAT;
{int}{frac}{exp} yylval->float_value = strtod(yytext, NULL); return T_FLOAT;
\"               BEGIN(STRING); buffer_init(&buf); string_first_column = yylloc->first_column;
"/*"             BEGIN(COMMENT);
.                fprintf(stderr, "Invalid character %c\n", yytext[1]);
<INITIAL><<EOF>> return T_EOF;

<STRING>\\\"     buffer_add(&buf, '"');
<STRING>\\\/     buffer_add(&buf, '/');
<STRING>\\\\     buffer_add(&buf, '\\');
<STRING>\\b      buffer_add(&buf, '\b');
<STRING>\\f      buffer_add(&buf, '\f');
<STRING>\\n      buffer_add(&buf, '\n');
<STRING>\\r      buffer_add(&buf, '\r');
<STRING>\\t      buffer_add(&buf, '\t');
<STRING>\\u{nonSurrogateHex} {
    uint32_t u = hex4_decode(&yytext[2]);
    assert(u < 0xd800 || 0xdfff < u);
    buffer_addu(&buf, u);
}
<STRING>\\u{highSurrogateHex}\\u{lowSurrogateHex} {
    uint32_t u1 = hex4_decode(&yytext[2]);
    uint32_t u2 = hex4_decode(&yytext[8]);
    assert(0xd800 <= u1 && u1 <= 0xdbff);
    assert(0xdc00 <= u2 && u2 <= 0xdfff);
    uint32_t u = 0x10000 + ((u1 & 0x3ff) << 10) + (u2 & 0x3ff);
    buffer_addu(&buf, u);
}
<STRING>\\u{surrogateHex}     fprintf(stderr, "Invalid unicode escape: unmatched surrogate pair\n");
<STRING>\\.                   fprintf(stderr, "Invalid string escape %c\n", yytext[1]);
<STRING>\\                    ; // Backslash at EOF, let <STRING><<EOF>> handle it
<STRING>\n                    yylloc->line++; fprintf(stderr, "Invalid newline in string\n");
<STRING>[\0-\t\v-\x1F]        fprintf(stderr, "Invalid control character in string %d\n", (int)yytext[1]);
<STRING>{stringChar}+         buffer_adds(&buf, yytext, yyleng);
<STRING>{wtf8Surrogate}       fprintf(stderr, "Invalid utf8: unmatched surrogate pair\n");
<STRING>\"                    {
                                  BEGIN(INITIAL);
                                  yylval->string_value = buffer_extract(&buf);
                                  yylloc->first_column = string_first_column;
                                  return T_STRING;
                              }
<STRING><<EOF>>               {
                                  fprintf(stderr, "Unclosed string\n");
                                  BEGIN(INITIAL);
                                  yylval->string_value = buffer_extract(&buf);
                                  yylloc->first_column = string_first_column;
                                  return T_STRING;
                              }
<STRING>.{cont}*              fprintf(stderr, "Invalid utf8\n");

<COMMENT>"*/"       BEGIN(INITIAL);
<COMMENT>"*"        ;
<COMMENT>[^*\n]+    ;
<COMMENT>\n         yylloc->line++;
<COMMENT><<EOF>>    fprintf(stderr, "Unclosed comment\n"); return T_EOF;

%%

static void buffer_init(struct buffer_t *b) {
    b->buf = malloc(1024);
    b->size = 0;
    b->capacity = 1024;
    if (!b->buf)
        abort();
}

static void buffer_add(struct buffer_t *b, char c) {
    if (b->size == b->capacity) {
        b->capacity *= 2;
        char *p = realloc(b->buf, b->capacity);
        if (!p)
            abort();
        b->buf = p;
    }
    b->buf[b->size++] = c;
}

static void buffer_adds(struct buffer_t *b, char *s, size_t n) {
    if (b->size + n >= b->capacity) {
        while (b->size + n >= b->capacity)
            b->capacity *= 2;
        char *p = realloc(b->buf, b->capacity);
        if (!p)
            abort();
        b->buf = p;
    }
    memcpy(&b->buf[b->size], s, n);
    b->size += n;
}

static inline void buffer_addu(struct buffer_t *b, uint32_t u) {
    int enc_size;
    if (u < 0x80) {
        buffer_add(b, (char)u);
        return;
    } else if (u < 0x800) {
        enc_size = 2;
    } else if (u < 0x10000) {
        enc_size = 3;
    } else if (u < 0x110000) {
        enc_size = 4;
    } else {
        unreachable();
    }

    if (b->size + enc_size > b->capacity) {
        b->capacity *= 2;
        char *p = realloc(b->buf, b->capacity);
        if (!p)
            abort();
        b->buf = p;
    }

    switch (enc_size) {
        case 2:
            b->buf[b->size++] = (char)(((u >>  6) & 0x1f) | 0xc0);
            b->buf[b->size++] = (char)((u         & 0x3f) | 0x80);
            break;
        case 3:
            b->buf[b->size++] = (char)(((u >> 12) & 0x0f) | 0xe0);
            b->buf[b->size++] = (char)(((u >>  6) & 0x3f) | 0x80);
            b->buf[b->size++] = (char)((u         & 0x3f) | 0x80);
            break;
        case 4:
            b->buf[b->size++] = (char)(((u >> 18) & 0x07) | 0xf0);
            b->buf[b->size++] = (char)(((u >> 12) & 0x3f) | 0x80);
            b->buf[b->size++] = (char)(((u >>  6) & 0x3f) | 0x80);
            b->buf[b->size++] = (char)((u         & 0x3f) | 0x80);
            break;
        default:
            unreachable();
    }
}

static char *buffer_extract(struct buffer_t *b) {
    buffer_add(b, '\0');
    char *p = realloc(b->buf, b->size);
    if (!p) {
        return b->buf;
    }
    return p;
}

static int xdigit_value(char c) {
    if ('0' <= c && c <= '9') {
        return c - '0';
    } else if ('a' <= c && c <= 'f') {
        return 10 + c - 'a';
    } else if ('A' <= c && c <= 'F') {
        return 10 + c - 'A';
    } else {
        unreachable();
    }
}

static uint32_t hex4_decode(const char *str) {
    uint32_t u = xdigit_value(str[0]);
    u = u * 16 + xdigit_value(str[1]);
    u = u * 16 + xdigit_value(str[2]);
    u = u * 16 + xdigit_value(str[3]);
    return u;
}

