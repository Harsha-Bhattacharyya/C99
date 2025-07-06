// Copyright Harsha Bhattacharyya 2025

/* This file is part of the C++Script project.

The C++Script project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

The C++Script project is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with the C++Script project. If not, see <https://www.gnu.org/licenses/>. */

#include <stdio.h>
#include "lex.yy.c"
#include "pparser.tab.h"

// Function to map token codes to their string representations
const char* token_to_string(int token) {
    switch (token) {
        // Keywords
        case AUTO: return "AUTO";
        case BREAK: return "BREAK";
        case CASE: return "CASE";
        case CHAR: return "CHAR";
        case CONST: return "CONST";
        case CONTINUE: return "CONTINUE";
        case DEFAULT: return "DEFAULT";
        case DO: return "DO";
        case DOUBLE: return "DOUBLE";
        case ELSE: return "ELSE";
        case ENUM: return "ENUM";
        case EXTERN: return "EXTERN";
        case FLOAT: return "FLOAT";
        case FOR: return "FOR";
        case GOTO: return "GOTO";
        case IF: return "IF";
        case INLINE: return "INLINE";
        case INT: return "INT";
        case LONG: return "LONG";
        case REGISTER: return "REGISTER";
        case RESTRICT: return "RESTRICT";
        case RETURN: return "RETURN";
        case SHORT: return "SHORT";
        case SIGNED: return "SIGNED";
        case SIZEOF: return "SIZEOF";
        case STATIC: return "STATIC";
        case STRUCT: return "STRUCT";
        case SWITCH: return "SWITCH";
        case TYPEDEF: return "TYPEDEF";
        case UNION: return "UNION";
        case UNSIGNED: return "UNSIGNED";
        case VOID: return "VOID";
        case VOLATILE: return "VOLATILE";
        case WHILE: return "WHILE";
        case _BOOL: return "_BOOL";
        case _COMPLEX: return "_COMPLEX";
        case _IMAGINARY: return "_IMAGINARY";

        // Literals and Identifiers
        case IDENTIFIER: return "IDENTIFIER";
        case INTEGER_CONSTANT: return "INTEGER_CONSTANT";
        case FLOATING_CONSTANT: return "FLOATING_CONSTANT";
        case CHARACTER_CONSTANT: return "CHARACTER_CONSTANT";
        case STRING_LITERAL: return "STRING_LITERAL";
        case BOOL_LITERAL: return "BOOL_LITERAL";

        // Operators and Punctuators
        case ELLIPSIS: return "ELLIPSIS";
        case RIGHT_ASSIGN: return "RIGHT_ASSIGN";
        case LEFT_ASSIGN: return "LEFT_ASSIGN";
        case ADD_ASSIGN: return "ADD_ASSIGN";
        case SUB_ASSIGN: return "SUB_ASSIGN";
        case MUL_ASSIGN: return "MUL_ASSIGN";
        case DIV_ASSIGN: return "DIV_ASSIGN";
        case MOD_ASSIGN: return "MOD_ASSIGN";
        case AND_ASSIGN: return "AND_ASSIGN";
        case XOR_ASSIGN: return "XOR_ASSIGN";
        case OR_ASSIGN: return "OR_ASSIGN";
        case RIGHT_SHIFT: return "RIGHT_SHIFT";
        case LEFT_SHIFT: return "LEFT_SHIFT";
        case INC_OP: return "INC_OP";
        case DEC_OP: return "DEC_OP";
        case ARROW: return "ARROW";
        case AND_OP: return "AND_OP";
        case OR_OP: return "OR_OP";
        case LE_OP: return "LE_OP";
        case GE_OP: return "GE_OP";
        case EQ_OP: return "EQ_OP";
        case NE_OP: return "NE_OP";
        case HASH: return "HASH";
        case DOUBLE_HASH: return "DOUBLE_HASH";

        default:
            // For single-character tokens, return them as a string
            if (token > 0 && token < 256) {
                static char single_char[2];
                single_char[0] = (char)token;
                single_char[1] = '\0';
                return single_char;
            }
            return "UNKNOWN_TOKEN";
    }
}

int lex(int argc, char* argv[]) {
    if (argc > 1) {
        FILE* file = fopen(argv[1], "r");
        if (!file) {
            perror(argv[1]);
            return 1;
        }
        yyin = file;
    }

    printf("%-4s %-4s %-20s %-25s %s\n", "Line", "Col", "Token", "Value", "Text");
    printf("------------------------------------------------------------------------------\n");

    int token;
    while ((token = yylex())) {
        // For tokens that carry a string value, print it
        if (token == IDENTIFIER || token == INTEGER_CONSTANT || token == FLOATING_CONSTANT ||
            token == CHARACTER_CONSTANT || token == STRING_LITERAL || token == BOOL_LITERAL) {
            printf("%-4d %-4d %-20s %-25s %s\n", line_num, col_num, token_to_string(token), yylval.sval, yytext);
            free(yylval.sval); // Free the duplicated string
        } else {
            printf("%-4d %-4d %-20s %-25s %s\n", line_num, col_num, token_to_string(token), "", yytext);
        }
    }

    if (argc > 1) {
        fclose(yyin);
    }

    printf("\nLexical analysis complete. Total lines: %d\n", line_num - 1);
    return 0;
}




