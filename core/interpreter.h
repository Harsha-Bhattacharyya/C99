// Copyright Harsha Bhattacharyya 2025
// LLVM-based C99 interpreter implementation

#ifndef INTERPRETER_H
#define INTERPRETER_H

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

// Initialize the LLVM interpreter
void interpreter_init(void);

// Execute the generated LLVM IR
int interpreter_execute(void);

// Cleanup the interpreter
void interpreter_cleanup(void);

// Get the current LLVM context
LLVMContextRef get_llvm_context(void);

// Get the current LLVM module
LLVMModuleRef get_llvm_module(void);

// Get the current LLVM builder
LLVMBuilderRef get_llvm_builder(void);

// Generate LLVM IR for C99 constructs
LLVMValueRef gen_function(const char *name, LLVMTypeRef ret_type, LLVMTypeRef *param_types, int param_count);
LLVMBasicBlockRef gen_basic_block(LLVMValueRef function, const char *name);
LLVMValueRef gen_int_constant(int value, int bits);
LLVMValueRef gen_binary_op(const char *op, LLVMValueRef left, LLVMValueRef right);
LLVMValueRef gen_call(LLVMValueRef function, LLVMValueRef *args, int arg_count);
void gen_return(LLVMValueRef value);
void gen_return_void(void);

// Symbol table for variables
typedef struct llvm_symbol {
    char *name;
    LLVMValueRef value;
    LLVMTypeRef type;
    struct llvm_symbol *next;
} llvm_symbol_t;

void add_llvm_symbol(const char *name, LLVMValueRef value, LLVMTypeRef type);
llvm_symbol_t *lookup_llvm_symbol(const char *name);

#endif // INTERPRETER_H
