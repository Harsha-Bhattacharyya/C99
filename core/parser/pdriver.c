#include "pparser.tab.c"
#include <stdio.h>

extern FILE *yyin;

int pars(int argc, char *argv[]) {
    init_mlir_module();
    
    // Open input file if provided
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            perror(argv[1]);
            return 1;
        }
    }
    
    int result = yyparse();
    
    if (result == 0) {
        print_mlir_module();
    }
    
    cleanup_mlir_module();
    
    if (yyin && yyin != stdin) {
        fclose(yyin);
    }
    
    return result;
}
