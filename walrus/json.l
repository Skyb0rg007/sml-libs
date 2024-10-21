
%top {
#define _GNU_SOURCE

enum {
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
    T_FLOAT,
    T_EOF
};

typedef int YYLTYPE;
typedef union {
    char *string;
} YYSTYPE;
}

digit  [0-9]
digits {digit}+
int    "-"?([0-9]|[1-9][0-9]+)
frac   "."[0-9]+
exp    [eE][+-]?[0-9]+
xdigit [0-9a-fA-F]

%option noyywrap
%option reentrant
%option 8bit
%option bison-bridge bison-locations
%option ecs meta-ecs

%x STRING

%%

%{
char *buf;
char *cur;
%}

[\ \t\r\n]       ;
"{"              return T_LBRACE;
"}"              return T_RBRACE;
"["              return T_LBRACK;
"]"              return T_RBRACK;
","              return T_COMMA;
":"              return T_COLON;
"null"           return T_NULL;
"true"           return T_TRUE;
"false"          return T_FALSE;
{int}            yylval->string = yytext; return T_INT;
{int}{frac}      yylval->string = yytext; return T_FLOAT;
{int}{exp}       yylval->string = yytext; return T_FLOAT;
{int}{frac}{exp} yylval->string = yytext; return T_FLOAT;
\"               BEGIN(STRING); buf = cur = malloc(1024);
<<EOF>>          return T_EOF;

<STRING>\\\\           *cur++ = '\\';
<STRING>\\b            *cur++ = '\b';
<STRING>\\f            *cur++ = '\f';
<STRING>\\n            *cur++ = '\n';
<STRING>\\r            *cur++ = '\r';
<STRING>\\t            *cur++ = '\t';
<STRING>\\u{xdigit}{4} *cur++ = 'x';
<STRING>[^\\"]+        strcpy(cur, yytext); cur += yyleng;
<STRING>\"             BEGIN(INITIAL); yylval->string = buf; buf = cur = NULL; return T_STRING;

%%

int main(int argc, char **argv) {
    yyscan_t scanner;
    yylex_init(&scanner);

    YYSTYPE lval = {0};
    YYLTYPE lloc = 0;
    int tok;
    while ((tok = yylex(&lval, &lloc, scanner))) {
        switch (tok) {
            case T_LBRACE: { puts("LBRACE"); break; }
            case T_RBRACE: { puts("RBRACE"); break; }
            case T_LBRACK: { puts("LBRACK"); break; }
            case T_RBRACK: { puts("RBRACK"); break; }
            case T_COMMA:  { puts("COMMA"); break;  }
            case T_COLON:  { puts("COLON"); break;  }
            case T_NULL:   { puts("NULL"); break;   }
            case T_FALSE:  { puts("FALSE"); break;  }
            case T_TRUE:   { puts("TRUE"); break;   }
            case T_EOF:    { puts("EOF"); break;   }
            case T_STRING: { printf("STRING(\"%s\")\n", lval.string); free(lval.string); break; }
            case T_INT:    { printf("INT(%s)\n", lval.string); break;    }
            case T_FLOAT:  { printf("FLOAT(%s)\n", lval.string); break;  }
        }
    }

    yylex_destroy(scanner);
}


