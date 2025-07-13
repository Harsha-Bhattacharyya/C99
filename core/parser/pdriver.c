#include "pparser.tab.c"

int pars(int argc, char *argv[]) {
    init_mlir_module();
    
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
