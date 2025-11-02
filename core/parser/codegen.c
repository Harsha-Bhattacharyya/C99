// Code generation implementation for LLVM IR
#include "codegen.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

CodeGenContext *codegen_ctx = NULL;
static int temp_counter = 0;

// Symbol table for variables
typedef struct Symbol {
    char *name;
    LLVMValueRef alloca;
    LLVMTypeRef type;
    struct Symbol *next;
} Symbol;

static Symbol *symbol_table = NULL;

void codegen_init(void) {
    codegen_ctx = malloc(sizeof(CodeGenContext));
    codegen_ctx->current_function = NULL;
    codegen_ctx->current_block = NULL;
    codegen_ctx->break_block = NULL;
    codegen_ctx->continue_block = NULL;
    symbol_table = NULL;
    temp_counter = 0;
}

void codegen_cleanup(void) {
    // Clean up symbol table
    Symbol *sym = symbol_table;
    while (sym) {
        Symbol *next = sym->next;
        free(sym->name);
        free(sym);
        sym = next;
    }
    symbol_table = NULL;
    
    if (codegen_ctx) {
        free(codegen_ctx);
        codegen_ctx = NULL;
    }
}

char *gen_temp_name(void) {
    char *name = malloc(32);
    snprintf(name, 32, "tmp%d", temp_counter++);
    return name;
}

LLVMTypeRef get_c_type(const char *type_name) {
    LLVMContextRef ctx = get_llvm_context();
    
    if (strcmp(type_name, "void") == 0) return LLVMVoidTypeInContext(ctx);
    if (strcmp(type_name, "char") == 0 || strcmp(type_name, "i8") == 0) 
        return LLVMInt8TypeInContext(ctx);
    if (strcmp(type_name, "short") == 0 || strcmp(type_name, "i16") == 0) 
        return LLVMInt16TypeInContext(ctx);
    if (strcmp(type_name, "int") == 0 || strcmp(type_name, "i32") == 0) 
        return LLVMInt32TypeInContext(ctx);
    if (strcmp(type_name, "long") == 0 || strcmp(type_name, "i64") == 0) 
        return LLVMInt64TypeInContext(ctx);
    if (strcmp(type_name, "float") == 0 || strcmp(type_name, "f32") == 0) 
        return LLVMFloatTypeInContext(ctx);
    if (strcmp(type_name, "double") == 0 || strcmp(type_name, "f64") == 0) 
        return LLVMDoubleTypeInContext(ctx);
    
    return LLVMInt32TypeInContext(ctx); // Default
}

ExprValue codegen_integer_constant(long long value) {
    ExprValue result;
    result.value = LLVMConstInt(LLVMInt32TypeInContext(get_llvm_context()), value, 1);
    result.type = LLVMInt32TypeInContext(get_llvm_context());
    return result;
}

ExprValue codegen_float_constant(double value) {
    ExprValue result;
    result.value = LLVMConstReal(LLVMDoubleTypeInContext(get_llvm_context()), value);
    result.type = LLVMDoubleTypeInContext(get_llvm_context());
    return result;
}

ExprValue codegen_binary_op(const char *op, ExprValue left, ExprValue right) {
    LLVMBuilderRef builder = get_llvm_builder();
    ExprValue result;
    char *name = gen_temp_name();
    
    LLVMTypeKind kind = LLVMGetTypeKind(left.type);
    int is_float = (kind == LLVMFloatTypeKind || kind == LLVMDoubleTypeKind);
    
    if (strcmp(op, "+") == 0) {
        result.value = is_float ? 
            LLVMBuildFAdd(builder, left.value, right.value, name) :
            LLVMBuildAdd(builder, left.value, right.value, name);
    } else if (strcmp(op, "-") == 0) {
        result.value = is_float ?
            LLVMBuildFSub(builder, left.value, right.value, name) :
            LLVMBuildSub(builder, left.value, right.value, name);
    } else if (strcmp(op, "*") == 0) {
        result.value = is_float ?
            LLVMBuildFMul(builder, left.value, right.value, name) :
            LLVMBuildMul(builder, left.value, right.value, name);
    } else if (strcmp(op, "/") == 0) {
        result.value = is_float ?
            LLVMBuildFDiv(builder, left.value, right.value, name) :
            LLVMBuildSDiv(builder, left.value, right.value, name);
    } else if (strcmp(op, "%") == 0) {
        result.value = LLVMBuildSRem(builder, left.value, right.value, name);
    } else if (strcmp(op, "<") == 0) {
        result.value = is_float ?
            LLVMBuildFCmp(builder, LLVMRealOLT, left.value, right.value, name) :
            LLVMBuildICmp(builder, LLVMIntSLT, left.value, right.value, name);
        result.type = LLVMInt1TypeInContext(get_llvm_context());
        free(name);
        return result;
    } else if (strcmp(op, ">") == 0) {
        result.value = is_float ?
            LLVMBuildFCmp(builder, LLVMRealOGT, left.value, right.value, name) :
            LLVMBuildICmp(builder, LLVMIntSGT, left.value, right.value, name);
        result.type = LLVMInt1TypeInContext(get_llvm_context());
        free(name);
        return result;
    } else if (strcmp(op, "<=") == 0) {
        result.value = is_float ?
            LLVMBuildFCmp(builder, LLVMRealOLE, left.value, right.value, name) :
            LLVMBuildICmp(builder, LLVMIntSLE, left.value, right.value, name);
        result.type = LLVMInt1TypeInContext(get_llvm_context());
        free(name);
        return result;
    } else if (strcmp(op, ">=") == 0) {
        result.value = is_float ?
            LLVMBuildFCmp(builder, LLVMRealOGE, left.value, right.value, name) :
            LLVMBuildICmp(builder, LLVMIntSGE, left.value, right.value, name);
        result.type = LLVMInt1TypeInContext(get_llvm_context());
        free(name);
        return result;
    } else if (strcmp(op, "==") == 0) {
        result.value = is_float ?
            LLVMBuildFCmp(builder, LLVMRealOEQ, left.value, right.value, name) :
            LLVMBuildICmp(builder, LLVMIntEQ, left.value, right.value, name);
        result.type = LLVMInt1TypeInContext(get_llvm_context());
        free(name);
        return result;
    } else if (strcmp(op, "!=") == 0) {
        result.value = is_float ?
            LLVMBuildFCmp(builder, LLVMRealONE, left.value, right.value, name) :
            LLVMBuildICmp(builder, LLVMIntNE, left.value, right.value, name);
        result.type = LLVMInt1TypeInContext(get_llvm_context());
        free(name);
        return result;
    } else if (strcmp(op, "&") == 0) {
        result.value = LLVMBuildAnd(builder, left.value, right.value, name);
    } else if (strcmp(op, "|") == 0) {
        result.value = LLVMBuildOr(builder, left.value, right.value, name);
    } else if (strcmp(op, "^") == 0) {
        result.value = LLVMBuildXor(builder, left.value, right.value, name);
    } else if (strcmp(op, "<<") == 0) {
        result.value = LLVMBuildShl(builder, left.value, right.value, name);
    } else if (strcmp(op, ">>") == 0) {
        result.value = LLVMBuildAShr(builder, left.value, right.value, name);
    } else {
        // Unknown operator, return left value
        result.value = left.value;
        result.type = left.type;
        free(name);
        return result;
    }
    
    result.type = left.type;
    free(name);
    return result;
}

void codegen_return(ExprValue value) {
    LLVMBuildRet(get_llvm_builder(), value.value);
}

void codegen_return_void(void) {
    LLVMBuildRetVoid(get_llvm_builder());
}

void codegen_function_start(const char *name, LLVMTypeRef ret_type,
                            LLVMTypeRef *param_types, int param_count) {
    LLVMTypeRef func_type = LLVMFunctionType(ret_type, param_types, param_count, 0);
    codegen_ctx->current_function = LLVMAddFunction(get_llvm_module(), name, func_type);
    
    LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(get_llvm_context(), 
                                                             codegen_ctx->current_function, "entry");
    codegen_ctx->current_block = entry;
    LLVMPositionBuilderAtEnd(get_llvm_builder(), entry);
}

void codegen_function_end(void) {
    // Add default return if needed
    if (!LLVMGetBasicBlockTerminator(codegen_ctx->current_block)) {
        LLVMTypeRef func_type = LLVMGlobalGetValueType(codegen_ctx->current_function);
        LLVMTypeRef ret_type = LLVMGetReturnType(func_type);
        
        if (LLVMGetTypeKind(ret_type) == LLVMVoidTypeKind) {
            LLVMBuildRetVoid(get_llvm_builder());
        } else {
            LLVMBuildRet(get_llvm_builder(), LLVMConstInt(ret_type, 0, 0));
        }
    }
    
    codegen_ctx->current_function = NULL;
    codegen_ctx->current_block = NULL;
}

LLVMValueRef codegen_create_variable(const char *name, LLVMTypeRef type) {
    LLVMBuilderRef temp_builder = LLVMCreateBuilderInContext(get_llvm_context());
    LLVMBasicBlockRef entry = LLVMGetEntryBasicBlock(codegen_ctx->current_function);
    LLVMValueRef first_inst = LLVMGetFirstInstruction(entry);
    
    if (first_inst) {
        LLVMPositionBuilderBefore(temp_builder, first_inst);
    } else {
        LLVMPositionBuilderAtEnd(temp_builder, entry);
    }
    
    LLVMValueRef alloca = LLVMBuildAlloca(temp_builder, type, name);
    LLVMDisposeBuilder(temp_builder);
    
    // Add to symbol table
    Symbol *sym = malloc(sizeof(Symbol));
    sym->name = strdup(name);
    sym->alloca = alloca;
    sym->type = type;
    sym->next = symbol_table;
    symbol_table = sym;
    
    return alloca;
}

ExprValue codegen_load_variable(const char *name) {
    // Find variable in symbol table
    Symbol *sym = symbol_table;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            ExprValue result;
            char *temp = gen_temp_name();
            result.value = LLVMBuildLoad2(get_llvm_builder(), sym->type, sym->alloca, temp);
            result.type = sym->type;
            free(temp);
            return result;
        }
        sym = sym->next;
    }
    
    // Variable not found, return zero
    return codegen_integer_constant(0);
}

void codegen_store_variable(const char *name, ExprValue value) {
    Symbol *sym = symbol_table;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            LLVMBuildStore(get_llvm_builder(), value.value, sym->alloca);
            return;
        }
        sym = sym->next;
    }
}

ExprValue codegen_assign(ExprValue lhs, ExprValue rhs) {
    // For now, assume lhs is a variable name stored as pointer
    // This is simplified - real implementation would track lvalues properly
    return rhs;
}
