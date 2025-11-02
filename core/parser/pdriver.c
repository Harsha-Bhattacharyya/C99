#include "pparser.tab.c"
#include "../interpreter.h"
#include "codegen.h"
#include <stdio.h>

extern FILE *yyin;

int pars(int argc, char *argv[]) {
    // Initialize LLVM interpreter
    interpreter_init();
    
    // Initialize code generation
    codegen_init();
    
    // Open input file if provided
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            perror(argv[1]);
            codegen_cleanup();
            interpreter_cleanup();
            return 1;
        }
    }
    
    int result = yyparse();
    
    if (result == 0) {
        // Execute the code instead of just printing
        result = interpreter_execute();
    }
    
    codegen_cleanup();
    interpreter_cleanup();
    
    if (yyin && yyin != stdin) {
        fclose(yyin);
    }
    
    return result;
}
