
#include "json.yy.h"

int main(void)
{
    yyscan_t scanner;
    yylex_init(&scanner);
    yyset_in(stdin, scanner);

    int tok;
    YYSTYPE lval;
    YYLTYPE lloc = {
        .line = 1,
        .first_column = 1,
        .last_column = 1
    };
    while ((tok = yylex(&lval, &lloc, scanner))) {
        printf("%d.%d-%d: ", lloc.line, lloc.first_column, lloc.last_column);
        switch (tok) {
            case T_LBRACE: printf("LBRACE\n"); break;
            case T_RBRACE: printf("RBRACE\n"); break;
            case T_LBRACK: printf("LBRACK\n"); break;
            case T_RBRACK: printf("RBRACK\n"); break;
            case T_COMMA: printf("COMMA\n"); break;
            case T_COLON: printf("COLON\n"); break;
            case T_NULL: printf("NULL\n"); break;
            case T_FALSE: printf("FALSE\n"); break;
            case T_TRUE: printf("TRUE\n"); break;
            case T_STRING: printf("STRING(\"%s\")\n", lval.string_value); free(lval.string_value); break;
            case T_INT: printf("INT(%s)\n", lval.string_value); break;
            case T_FLOAT: printf("FLOAT(%s)\n", lval.string_value); break;
        }
    }
    printf("EOF\n");
    yylex_destroy(scanner);
}
