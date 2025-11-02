# Copyright Harsha Bhattacharyya 2025
#
# This file is part of the C++Script project.
#
# The C++Script project is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or any later version.
#
# The C++Script project is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# the C++Script project. If not, see <https://www.gnu.org/licenses/>.

## Build Targets

# Define some variables for clarity
FLEX_SOURCE = core/lexer/llexer.l
FLEX_OUTPUT = core/lexer/lex.yy.c
BISON_SOURCE = core/parser/pparser.y
BISON_OUTPUT_C = core/parser/pparser.tab.c
BISON_OUTPUT_H = core/parser/pparser.tab.h
C_SOURCES = core/lexer/ldriver.c core/parser/pdriver.c core/main.c
EXECUTABLE = cc

.PHONY: all lexer parser clean install

all: $(EXECUTABLE)

lexer: $(FLEX_SOURCE)
	flex -o $(FLEX_OUTPUT) $(FLEX_SOURCE)

parser: $(BISON_SOURCE)
	yacc -d -o $(BISON_OUTPUT_C) $(BISON_SOURCE)

$(EXECUTABLE): lexer parser $(C_SOURCES)
	clang -lfl -Icore/parser $(C_SOURCES) -o $(EXECUTABLE)

install: all
	cp $(EXECUTABLE) /usr/local/bin

clean:
	rm -f $(EXECUTABLE) $(FLEX_OUTPUT) $(BISON_OUTPUT_C) $(BISON_OUTPUT_H)
