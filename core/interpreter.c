// Copyright Harsha Bhattacharyya 2025
// LLVM-based C99 interpreter implementation

#include "interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Global LLVM state
static LLVMContextRef context = NULL;
static LLVMModuleRef module = NULL;
static LLVMBuilderRef builder = NULL;
static LLVMExecutionEngineRef engine = NULL;

// Symbol table
static llvm_symbol_t *symbol_table_head = NULL;

void interpreter_init(void) {
    // Initialize LLVM
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
    
    // Create context, module, and builder
    context = LLVMContextCreate();
    module = LLVMModuleCreateWithNameInContext("C99Interpreter", context);
    builder = LLVMCreateBuilderInContext(context);
    
    // Clear symbol table
    symbol_table_head = NULL;
}

int interpreter_execute(void) {
    // Verify the module
    char *error = NULL;
    if (LLVMVerifyModule(module, LLVMReturnStatusAction, &error)) {
        fprintf(stderr, "Module verification failed: %s\n", error);
        LLVMDisposeMessage(error);
        return 1;
    }
    
    // Print LLVM IR to stdout
    char *ir = LLVMPrintModuleToString(module);
    printf("Generated LLVM IR:\n%s\n", ir);
    LLVMDisposeMessage(ir);
    
    // Create execution engine
    if (LLVMCreateExecutionEngineForModule(&engine, module, &error)) {
        fprintf(stderr, "Failed to create execution engine: %s\n", error);
        LLVMDisposeMessage(error);
        return 1;
    }
    
    // Find the main function
    LLVMValueRef main_func = LLVMGetNamedFunction(module, "main");
    if (!main_func) {
        fprintf(stderr, "No main function found\n");
        return 1;
    }
    
    // Execute main function
    LLVMGenericValueRef result = LLVMRunFunction(engine, main_func, 0, NULL);
    int ret_val = (int)LLVMGenericValueToInt(result, 0);
    
    printf("\nProgram returned: %d\n", ret_val);
    
    LLVMDisposeGenericValue(result);
    
    return 0;
}

void interpreter_cleanup(void) {
    // Clean up symbol table
    llvm_symbol_t *sym = symbol_table_head;
    while (sym) {
        llvm_symbol_t *next = sym->next;
        free(sym->name);
        free(sym);
        sym = next;
    }
    
    // Clean up LLVM resources
    if (engine) {
        LLVMDisposeExecutionEngine(engine);
    } else if (module) {
        LLVMDisposeModule(module);
    }
    
    if (builder) {
        LLVMDisposeBuilder(builder);
    }
    
    if (context) {
        LLVMContextDispose(context);
    }
}

LLVMContextRef get_llvm_context(void) {
    return context;
}

LLVMModuleRef get_llvm_module(void) {
    return module;
}

LLVMBuilderRef get_llvm_builder(void) {
    return builder;
}

LLVMValueRef gen_function(const char *name, LLVMTypeRef ret_type, LLVMTypeRef *param_types, int param_count) {
    LLVMTypeRef func_type = LLVMFunctionType(ret_type, param_types, param_count, 0);
    LLVMValueRef function = LLVMAddFunction(module, name, func_type);
    return function;
}

LLVMBasicBlockRef gen_basic_block(LLVMValueRef function, const char *name) {
    return LLVMAppendBasicBlockInContext(context, function, name);
}

LLVMValueRef gen_int_constant(int value, int bits) {
    return LLVMConstInt(LLVMIntTypeInContext(context, bits), value, 0);
}

LLVMValueRef gen_binary_op(const char *op, LLVMValueRef left, LLVMValueRef right) {
    if (strcmp(op, "+") == 0) {
        return LLVMBuildAdd(builder, left, right, "addtmp");
    } else if (strcmp(op, "-") == 0) {
        return LLVMBuildSub(builder, left, right, "subtmp");
    } else if (strcmp(op, "*") == 0) {
        return LLVMBuildMul(builder, left, right, "multmp");
    } else if (strcmp(op, "/") == 0) {
        return LLVMBuildSDiv(builder, left, right, "divtmp");
    } else if (strcmp(op, "%") == 0) {
        return LLVMBuildSRem(builder, left, right, "modtmp");
    } else if (strcmp(op, "<") == 0) {
        return LLVMBuildICmp(builder, LLVMIntSLT, left, right, "cmptmp");
    } else if (strcmp(op, ">") == 0) {
        return LLVMBuildICmp(builder, LLVMIntSGT, left, right, "cmptmp");
    } else if (strcmp(op, "<=") == 0) {
        return LLVMBuildICmp(builder, LLVMIntSLE, left, right, "cmptmp");
    } else if (strcmp(op, ">=") == 0) {
        return LLVMBuildICmp(builder, LLVMIntSGE, left, right, "cmptmp");
    } else if (strcmp(op, "==") == 0) {
        return LLVMBuildICmp(builder, LLVMIntEQ, left, right, "cmptmp");
    } else if (strcmp(op, "!=") == 0) {
        return LLVMBuildICmp(builder, LLVMIntNE, left, right, "cmptmp");
    }
    
    // Default: return left operand
    return left;
}

LLVMValueRef gen_call(LLVMValueRef function, LLVMValueRef *args, int arg_count) {
    return LLVMBuildCall2(builder, LLVMGetElementType(LLVMTypeOf(function)), function, args, arg_count, "calltmp");
}

void gen_return(LLVMValueRef value) {
    LLVMBuildRet(builder, value);
}

void gen_return_void(void) {
    LLVMBuildRetVoid(builder);
}

void add_llvm_symbol(const char *name, LLVMValueRef value, LLVMTypeRef type) {
    llvm_symbol_t *sym = malloc(sizeof(llvm_symbol_t));
    sym->name = strdup(name);
    sym->value = value;
    sym->type = type;
    sym->next = symbol_table_head;
    symbol_table_head = sym;
}

llvm_symbol_t *lookup_llvm_symbol(const char *name) {
    llvm_symbol_t *sym = symbol_table_head;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            return sym;
        }
        sym = sym->next;
    }
    return NULL;
}
