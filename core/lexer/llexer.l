// Copyright Harsha Bhattacharyya 2025

/* This file is part of the C++Script project.

The C++Script project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

The C++Script project is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with the C++Script project. If not, see <https://www.gnu.org/licenses/>. */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
    // Literals
    INTEGER_CONSTANT = 256,
    FLOATING_CONSTANT,
    CHARACTER_CONSTANT,
    STRING_LITERAL,
    BOOL_LITERAL,

    // Identifiers and keywords
    IDENTIFIER,

    // Keywords
    AUTO, BREAK, CASE, CHAR, CONST, CONTINUE, DEFAULT, DO,
    DOUBLE, ELSE, ENUM, EXTERN, FLOAT, FOR, GOTO, IF,
    INLINE, INT, LONG, REGISTER, RESTRICT, RETURN, SHORT, SIGNED,
    SIZEOF, STATIC, STRUCT, SWITCH, TYPEDEF, UNION, UNSIGNED, VOID,
    VOLATILE, WHILE,

    // C99 keywords
    _BOOL, _COMPLEX, _IMAGINARY,

    // Operators
    ARROW, INC_OP, DEC_OP, LEFT_SHIFT, RIGHT_SHIFT,
    LE_OP, GE_OP, EQ_OP, NE_OP, AND_OP, OR_OP,
    MUL_ASSIGN, DIV_ASSIGN, MOD_ASSIGN, ADD_ASSIGN, SUB_ASSIGN,
    LEFT_ASSIGN, RIGHT_ASSIGN, AND_ASSIGN, XOR_ASSIGN, OR_ASSIGN,

    // Punctuators
    ELLIPSIS,

    // Preprocessor
    HASH, DOUBLE_HASH
};

int line_num = 1;
int col_num = 1;

void count_position() {
    int i;
    for (i = 0; yytext[i] != '\0'; i++) {
        if (yytext[i] == '\n') {
            line_num++;
            col_num = 1;
        } else {
            col_num++;
        }
    }
}

void print_token(int token, const char* name) {
    printf("Line %d, Col %d: %s '%s'\n", line_num, col_num, name, yytext);
}

%}

%option noyywrap
%option yylineno

/* Regular expression definitions */
D               [0-9]
L               [a-zA-Z_]
H               [a-fA-F0-9]
E               ([Ee][+-]?{D}+)
P               ([Pp][+-]?{D}+)
FS              (f|F|l|L)
IS              ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))

/* Whitespace */
WS              [ \t\v\f]

%%

"/*"            { /* C-style comment */
                  int c;
                  while ((c = input()) != 0) {
                      if (c == '\n') {
                          line_num++;
                          col_num = 1;
                      } else {
                          col_num++;
                      }
                      if (c == '*') {
                          if ((c = input()) == '/') {
                              col_num++;
                              break;
                          } else {
                              unput(c);
                          }
                      }
                  }
                }

"//".*          { /* C++-style comment */ }

"auto"          { count_position(); print_token(AUTO, "KEYWORD"); return AUTO; }
"break"         { count_position(); print_token(BREAK, "KEYWORD"); return BREAK; }
"case"          { count_position(); print_token(CASE, "KEYWORD"); return CASE; }
"char"          { count_position(); print_token(CHAR, "KEYWORD"); return CHAR; }
"const"         { count_position(); print_token(CONST, "KEYWORD"); return CONST; }
"continue"      { count_position(); print_token(CONTINUE, "KEYWORD"); return CONTINUE; }
"default"       { count_position(); print_token(DEFAULT, "KEYWORD"); return DEFAULT; }
"do"            { count_position(); print_token(DO, "KEYWORD"); return DO; }
"double"        { count_position(); print_token(DOUBLE, "KEYWORD"); return DOUBLE; }
"else"          { count_position(); print_token(ELSE, "KEYWORD"); return ELSE; }
"enum"          { count_position(); print_token(ENUM, "KEYWORD"); return ENUM; }
"extern"        { count_position(); print_token(EXTERN, "KEYWORD"); return EXTERN; }
"float"         { count_position(); print_token(FLOAT, "KEYWORD"); return FLOAT; }
"for"           { count_position(); print_token(FOR, "KEYWORD"); return FOR; }
"goto"          { count_position(); print_token(GOTO, "KEYWORD"); return GOTO; }
"if"            { count_position(); print_token(IF, "KEYWORD"); return IF; }
"inline"        { count_position(); print_token(INLINE, "KEYWORD"); return INLINE; }
"int"           { count_position(); print_token(INT, "KEYWORD"); return INT; }
"long"          { count_position(); print_token(LONG, "KEYWORD"); return LONG; }
"register"      { count_position(); print_token(REGISTER, "KEYWORD"); return REGISTER; }
"restrict"      { count_position(); print_token(RESTRICT, "KEYWORD"); return RESTRICT; }
"return"        { count_position(); print_token(RETURN, "KEYWORD"); return RETURN; }
"short"         { count_position(); print_token(SHORT, "KEYWORD"); return SHORT; }
"signed"        { count_position(); print_token(SIGNED, "KEYWORD"); return SIGNED; }
"sizeof"        { count_position(); print_token(SIZEOF, "KEYWORD"); return SIZEOF; }
"static"        { count_position(); print_token(STATIC, "KEYWORD"); return STATIC; }
"struct"        { count_position(); print_token(STRUCT, "KEYWORD"); return STRUCT; }
"switch"        { count_position(); print_token(SWITCH, "KEYWORD"); return SWITCH; }
"typedef"       { count_position(); print_token(TYPEDEF, "KEYWORD"); return TYPEDEF; }
"union"         { count_position(); print_token(UNION, "KEYWORD"); return UNION; }
"unsigned"      { count_position(); print_token(UNSIGNED, "KEYWORD"); return UNSIGNED; }
"void"          { count_position(); print_token(VOID, "KEYWORD"); return VOID; }
"volatile"      { count_position(); print_token(VOLATILE, "KEYWORD"); return VOLATILE; }
"while"         { count_position(); print_token(WHILE, "KEYWORD"); return WHILE; }

"_Bool"         { count_position(); print_token(_BOOL, "KEYWORD"); return _BOOL; }
"_Complex"      { count_position(); print_token(_COMPLEX, "KEYWORD"); return _COMPLEX; }
"_Imaginary"    { count_position(); print_token(_IMAGINARY, "KEYWORD"); return _IMAGINARY; }

{L}({L}|{D})*   { count_position(); print_token(IDENTIFIER, "IDENTIFIER"); return IDENTIFIER; }

0[xX]{H}+{IS}?          { count_position(); print_token(INTEGER_CONSTANT, "HEX_INTEGER"); return INTEGER_CONSTANT; }
0[0-7]*{IS}?            { count_position(); print_token(INTEGER_CONSTANT, "OCTAL_INTEGER"); return INTEGER_CONSTANT; }
[1-9]{D}*{IS}?          { count_position(); print_token(INTEGER_CONSTANT, "DECIMAL_INTEGER"); return INTEGER_CONSTANT; }

{D}+{E}{FS}?            { count_position(); print_token(FLOATING_CONSTANT, "FLOAT_EXP"); return FLOATING_CONSTANT; }
{D}*"."{D}+{E}?{FS}?    { count_position(); print_token(FLOATING_CONSTANT, "FLOAT_DECIMAL"); return FLOATING_CONSTANT; }
{D}+"."{D}*{E}?{FS}?    { count_position(); print_token(FLOATING_CONSTANT, "FLOAT_DECIMAL"); return FLOATING_CONSTANT; }
0[xX]{H}+{P}{FS}?       { count_position(); print_token(FLOATING_CONSTANT, "HEX_FLOAT"); return FLOATING_CONSTANT; }
0[xX]{H}*"."{H}+{P}{FS}?    { count_position(); print_token(FLOATING_CONSTANT, "HEX_FLOAT"); return FLOATING_CONSTANT; }
0[xX]{H}+"."{H}*{P}{FS}?    { count_position(); print_token(FLOATING_CONSTANT, "HEX_FLOAT"); return FLOATING_CONSTANT; }

L?'(\\.|[^\\'\n])+'     { count_position(); print_token(CHARACTER_CONSTANT, "CHARACTER"); return CHARACTER_CONSTANT; }

L?\"(\\.|[^\\"\n])*\"   { count_position(); print_token(STRING_LITERAL, "STRING"); return STRING_LITERAL; }

t|f|true|false      { count_position(); print_token(BOOL_LITERAL, "BOOLEAN"); return BOOL_LITERAL; }

"..."           { count_position(); print_token(ELLIPSIS, "ELLIPSIS"); return ELLIPSIS; }
">>="           { count_position(); print_token(RIGHT_ASSIGN, "RIGHT_ASSIGN"); return RIGHT_ASSIGN; }
"<<="           { count_position(); print_token(LEFT_ASSIGN, "LEFT_ASSIGN"); return LEFT_ASSIGN; }
"+="            { count_position(); print_token(ADD_ASSIGN, "ADD_ASSIGN"); return ADD_ASSIGN; }
"-="            { count_position(); print_token(SUB_ASSIGN, "SUB_ASSIGN"); return SUB_ASSIGN; }
"*="            { count_position(); print_token(MUL_ASSIGN, "MUL_ASSIGN"); return MUL_ASSIGN; }
"/="            { count_position(); print_token(DIV_ASSIGN, "DIV_ASSIGN"); return DIV_ASSIGN; }
"%="            { count_position(); print_token(MOD_ASSIGN, "MOD_ASSIGN"); return MOD_ASSIGN; }
"&="            { count_position(); print_token(AND_ASSIGN, "AND_ASSIGN"); return AND_ASSIGN; }
"^="            { count_position(); print_token(XOR_ASSIGN, "XOR_ASSIGN"); return XOR_ASSIGN; }
"|="            { count_position(); print_token(OR_ASSIGN, "OR_ASSIGN"); return OR_ASSIGN; }
">>"            { count_position(); print_token(RIGHT_SHIFT, "RIGHT_SHIFT"); return RIGHT_SHIFT; }
"<<"            { count_position(); print_token(LEFT_SHIFT, "LEFT_SHIFT"); return LEFT_SHIFT; }
"++"            { count_position(); print_token(INC_OP, "INCREMENT"); return INC_OP; }
"--"            { count_position(); print_token(DEC_OP, "DECREMENT"); return DEC_OP; }
"->"            { count_position(); print_token(ARROW, "ARROW"); return ARROW; }
"&&"            { count_position(); print_token(AND_OP, "LOGICAL_AND"); return AND_OP; }
"||"            { count_position(); print_token(OR_OP, "LOGICAL_OR"); return OR_OP; }
"<="            { count_position(); print_token(LE_OP, "LESS_EQUAL"); return LE_OP; }
">="            { count_position(); print_token(GE_OP, "GREATER_EQUAL"); return GE_OP; }
"=="            { count_position(); print_token(EQ_OP, "EQUAL"); return EQ_OP; }
"!="            { count_position(); print_token(NE_OP, "NOT_EQUAL"); return NE_OP; }

";"             { count_position(); print_token(';', "SEMICOLON"); return ';'; }
("{"|"<%")      { count_position(); print_token('{', "LEFT_BRACE"); return '{'; }
("}"|"%>")      { count_position(); print_token('}', "RIGHT_BRACE"); return '}'; }
","             { count_position(); print_token(',', "COMMA"); return ','; }
":"             { count_position(); print_token(':', "COLON"); return ':'; }
"="             { count_position(); print_token('=', "ASSIGN"); return '='; }
"("             { count_position(); print_token('(', "LEFT_PAREN"); return '('; }
")"             { count_position(); print_token(')', "RIGHT_PAREN"); return ')'; }
("["|"<:")      { count_position(); print_token('[', "LEFT_BRACKET"); return '['; }
("]"|":>")      { count_position(); print_token(']', "RIGHT_BRACKET"); return ']'; }
"."             { count_position(); print_token('.', "DOT"); return '.'; }
"&"             { count_position(); print_token('&', "AMPERSAND"); return '&'; }
"!"             { count_position(); print_token('!', "EXCLAMATION"); return '!'; }
"~"             { count_position(); print_token('~', "TILDE"); return '~'; }
"-"             { count_position(); print_token('-', "MINUS"); return '-'; }
"+"             { count_position(); print_token('+', "PLUS"); return '+'; }
"*"             { count_position(); print_token('*', "ASTERISK"); return '*'; }
"/"             { count_position(); print_token('/', "SLASH"); return '/'; }
"%"             { count_position(); print_token('%', "PERCENT"); return '%'; }
"<"             { count_position(); print_token('<', "LESS_THAN"); return '<'; }
">"             { count_position(); print_token('>', "GREATER_THAN"); return '>'; }
"^"             { count_position(); print_token('^', "CARET"); return '^'; }
"|"             { count_position(); print_token('|', "PIPE"); return '|'; }
"?"             { count_position(); print_token('?', "QUESTION"); return '?'; }

^{WS}*"#"{WS}*  { count_position(); print_token(HASH, "PREPROCESSOR"); return HASH; }
"##"            { count_position(); print_token(DOUBLE_HASH, "TOKEN_PASTE"); return DOUBLE_HASH; }

{WS}+           { count_position(); /* consume whitespace */ }
\n              { line_num++; col_num = 1; }

.               { count_position(); printf("Line %d, Col %d: Unknown character '%s'\n", line_num, col_num, yytext); }

%%

int main(int argc, char **argv) {
    if (argc > 1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            perror(argv[1]);
            return 1;
        }
        yyin = file;
    }

    printf("C99 Lexical Analysis\n");
    printf("====================\n");

    int token;
    while ((token = yylex()) != 0) {
        // Tokens are printed in the action code
    }

    if (argc > 1) {
        fclose(yyin);
    }

    printf("\nLexical analysis complete. Total lines: %d\n", line_num - 1);
    return 0;
}
