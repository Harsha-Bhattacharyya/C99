// Code generation helpers for LLVM IR generation
#ifndef CODEGEN_H
#define CODEGEN_H

#include "../interpreter.h"

// Expression value wrapper
typedef struct {
    LLVMValueRef value;
    LLVMTypeRef type;
} ExprValue;

// Code generation context
typedef struct {
    LLVMValueRef current_function;
    LLVMBasicBlockRef current_block;
    LLVMBasicBlockRef break_block;
    LLVMBasicBlockRef continue_block;
} CodeGenContext;

// Global context
extern CodeGenContext *codegen_ctx;

// Context management
void codegen_init(void);
void codegen_cleanup(void);

// Expression code generation
ExprValue codegen_binary_op(const char *op, ExprValue left, ExprValue right);
ExprValue codegen_unary_op(const char *op, ExprValue operand);
ExprValue codegen_integer_constant(long long value);
ExprValue codegen_float_constant(double value);
ExprValue codegen_variable_ref(const char *name);
ExprValue codegen_assign(ExprValue lhs, ExprValue rhs);

// Statement code generation
void codegen_return(ExprValue value);
void codegen_return_void(void);
void codegen_if(ExprValue condition, void (*then_fn)(void), void (*else_fn)(void));
void codegen_while(ExprValue (*cond_fn)(void), void (*body_fn)(void));

// Variable management
LLVMValueRef codegen_create_variable(const char *name, LLVMTypeRef type);
void codegen_store_variable(const char *name, ExprValue value);
ExprValue codegen_load_variable(const char *name);

// Function management
void codegen_function_start(const char *name, LLVMTypeRef ret_type, 
                           LLVMTypeRef *param_types, int param_count);
void codegen_function_end(void);

// Utility
LLVMTypeRef get_c_type(const char *type_name);
char *gen_temp_name(void);

#endif // CODEGEN_H
