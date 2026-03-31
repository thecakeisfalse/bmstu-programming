%option noyywrap bison-bridge bison-locations

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAG_IDENT   1
#define TAG_INTEGER 2
#define TAG_KEYWORD 3
#define TAG_TYPE    4

// Имена целых типов произвольной разрядности: int33, uint88, int1234 uint5, атрибут - число битов,
// для int - отрицательное, для uint - положительное (для приммера: -33, 88, -1234, 5)

char *tag_names[] = {
    "EOF", "IDENT", "INTEGER", "KEYWORD", "TYPE"
};

char *keyword_names[] = {
    "ON", "OFF", "**"  
};

struct Position {
    int row, col;
};

struct Fragment {
    struct Position start, follow;
};

typedef struct Fragment YYLTYPE;

void print_pos(struct Position *p) {
    printf("(%d,%d)", p->row, p->col);
}

void print_frag(struct Fragment *f) {
    print_pos(&f->start);
    printf("-");
    print_pos(&f->follow);
}

union Token {
    int ident;
    int keyword;
    long num;
};

typedef union Token YYSTYPE;

#define MAX_IDENTS 1024

char *idents[MAX_IDENTS];
int ident_count = 0;

int add_ident(const char *name) {
    int i;
    for (i = 0; i < ident_count; i++) {
        if (strcmp(idents[i], name) == 0) {
            return i;
        }
    }
    if (ident_count >= MAX_IDENTS) {
        fprintf(stderr, "identifier table overflow\n");
        exit(1);
    }
    idents[ident_count] = strdup(name);
    return ident_count++;
}


struct Position cur;

#define MAX_ERRORS 256

struct Error {
    struct Position pos;
    char *msg;
};

struct Error errors[MAX_ERRORS];
int error_count = 0;

void err(char *msg) {
    if (error_count >= MAX_ERRORS) {
        return;
    }
    errors[error_count].pos = cur;
    errors[error_count].msg = strdup(msg);
    error_count++;
}

void print_errors(void) {
    int i;
    for (i = 0; i < error_count; i++) {
        printf("Error ");
        print_pos(&errors[i].pos);
        printf(": %s\n", errors[i].msg);
    }
}

#define YY_USER_ACTION                      \
{                                           \
    int i;                                  \
    yylloc->start = cur;                    \
    for (i = 0; i < yyleng; i++) {         \
        if (yytext[i] == '\n') {            \
            cur.row++; cur.col = 1;         \
        } else {                            \
            cur.col++;                      \
        }                                   \
    }                                       \
    yylloc->follow = cur;                   \
}

void init_scanner() {
    cur.row = 1;
    cur.col = 1;
}

%}

IDENT   [A-Z]+[\+\-\*]*
NUMBER  \*|(\+)+|(\-)+

%%

"ON"     { yylval->keyword = 0; return TAG_KEYWORD; }
"OFF"    { yylval->keyword = 1; return TAG_KEYWORD; }
"**"     { yylval->keyword = 2; return TAG_KEYWORD; }

uint[0-9]+ {
    yylval->num = atol(yytext + 4);
    return TAG_TYPE;
}

int[0-9]+ {
    yylval->num = -atol(yytext + 3);
    return TAG_TYPE;
}

{IDENT}  {
    yylval->ident = add_ident(yytext);
    return TAG_IDENT;
}

{NUMBER} {
    if (yytext[0] == '*') {
        yylval->num = 0;
    } else if (yytext[0] == '+') {
        yylval->num = (long)yyleng;
    } else {
        yylval->num = -(long)yyleng;
    }
    return TAG_INTEGER;
}

[\+\-\*]+ { err("mixed number sequence"); }

[\n\t ]+

. { err("unexpected character"); }

%%

int main(int argc, char* argv[]) {
    int tag;
    YYSTYPE value;
    YYLTYPE coords;

    if (argc < 2) {
        fprintf(stderr, "expected file\n");
        return 1;
    }

    FILE *f = fopen(argv[1], "r");
    if (!f) {
        perror(argv[1]);
        return 1;
    }
    
    yyin = f;
    init_scanner();

    do {
        tag = yylex(&value, &coords);
        if (tag != 0) {
            printf("%s", tag_names[tag]);
            print_frag(&coords);
            printf(": ");
            switch (tag) {
            case TAG_KEYWORD:
                printf("%s", keyword_names[value.keyword]);
                break;
            case TAG_IDENT:
                printf("\"%s\" (%d)", idents[value.ident], value.ident);
                break;
            case TAG_INTEGER:
                printf("%ld", value.num);
                break;
            case TAG_TYPE:
                printf("%ld", value.num);
                break;
            default:
                break;
            }
            printf("\n");
        }
    } while (tag != 0);

    printf("\n");
    if (error_count > 0) {
        print_errors();
    }

    fclose(f);

    return 0;
}
