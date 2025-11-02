# C99 Interpreter

A C99 interpreter built using LLVM, Bison, and Flex.

## Overview

This project implements an interpreter for a subset of the C99 programming language. It uses:
- **Flex** for lexical analysis (tokenization)
- **Bison** for parsing C99 syntax
- **LLVM** for code generation and Just-In-Time (JIT) execution

## Building

Requirements:
- `flex` - Fast Lexical Analyzer generator
- `yacc` or `bison` - Parser generator
- `clang` - C compiler
- `llvm-18-dev` - LLVM development libraries
- `libfl-dev` - Flex library

Build the interpreter:
```bash
make
```

Clean build artifacts:
```bash
make clean
```

## Usage

Run a C99 program:
```bash
./cc program.c
```

The interpreter will:
1. Parse the C code
2. Generate LLVM IR (Intermediate Representation)
3. Execute the code using LLVM's JIT engine
4. Display the program's return value

## Example

test_simple.c:
```c
int main() {
    return 0;
}
```

Run it:
```bash
./cc test_simple.c
```

Output:
```
Generated LLVM IR:
; ModuleID = 'C99Interpreter'
source_filename = "C99Interpreter"

define i32 @main() {
entry:
  ret i32 0
}

Program returned: 0
Compilation successful
```

## Current Limitations

- Preprocessor directives (#include, #define, etc.) are not fully supported
- C standard library functions (printf, scanf, etc.) require linking
- Only basic C99 syntax is supported
- Expression evaluation is partially implemented

## Architecture

1. **Lexer** (`core/lexer/llexer.l`): Tokenizes C99 source code
2. **Parser** (`core/parser/pparser.y`): Parses tokens into an AST and generates LLVM IR
3. **Interpreter** (`core/interpreter.c`): Manages LLVM context and JIT execution
4. **Main** (`core/main.c`): Entry point that coordinates lexing, parsing, and execution

## License

See LICENSE file for details.
