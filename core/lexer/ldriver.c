// Copyright Harsha Bhattacharyya 2025

/* This file is part of the C++Script project.

The C++Script project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

The C++Script project is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with the C++Script project. If not, see <https://www.gnu.org/licenses/>. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lex.yy.c" // flex file


int lex(int argc, char *argv[]) {
    if (argc > 1) {
        FILE *file = fopen(argv[1], "r");
        if (!file) {
            perror(argv[1]);
            return 1;
        }
        yyin = file;
    }


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



