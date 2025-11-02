# Complete Implementation Guide

## Overview
This document outlines the complete implementation strategy for transforming the C99 interpreter from stubs to fully functional LLVM IR generation.

## Current Architecture

### What We Have
- ✅ **Lexer** (Flex): Complete C99 tokenization
- ✅ **Parser** (Bison): Complete C99 grammar
- ✅ **LLVM Integration**: Basic setup with JIT
- ✅ **Codegen Infrastructure**: Clean abstraction layer (codegen.h/c)
- ⚠️  **Semantic Actions**: 63 stub calls, 42 temp generation calls

### What Needs Completion
1. Replace all `emit_operation()` stubs with real LLVM IR generation
2. Update parser union to use `ExprValue` instead of string-based values
3. Implement control flow IR generation
4. Add preprocessor
5. Add standard library function declarations
6. Add optimization passes
7. Create benchmark suite

## Implementation Strategy

### Phase 1: Core Expressions (Priority: CRITICAL)

#### Step 1: Update Parser Data Structures
```c
// In pparser.y, change union from:
struct {
    char *value;
    char *type;
} expr;

// To:
ExprValue expr;  // Defined in codegen.h
```

#### Step 2: Replace Primary Expressions
```c
// Old (stub):
INTEGER_CONSTANT
{
    char *temp = gen_temp();
    emit_operation("  %s = arith.constant %d : i32", temp, $1);
    $$.value = temp;
    $$.type = strdup("i32");
}

// New (working):
INTEGER_CONSTANT
{
    $$ = codegen_integer_constant($1);
}
```

#### Step 3: Replace Binary Operations
```c
// Old (stub):
additive_expression '+' multiplicative_expression
{
    char *temp = gen_temp();
    emit_operation("  %s = arith.addi %s, %s : %s", ...);
    $$.value = temp;
    ...
}

// New (working):
additive_expression '+' multiplicative_expression
{
    $$ = codegen_binary_op("+", $1, $3);
}
```

#### Step 4: Replace Statement Generation
```c
// Old (stub):
RETURN expression ';'
{
    emit_operation("  return %s : %s", $2.value, $2.type);
}

// New (working):
RETURN expression ';'
{
    codegen_return($2);
}
```

### Phase 2: Control Flow

#### If/Else Implementation
```c
// Codegen helper:
void codegen_if_else(ExprValue cond, 
                     LLVMBasicBlockRef then_block,
                     LLVMBasicBlockRef else_block,
                     LLVMBasicBlockRef merge_block);

// Parser action:
IF '(' expression ')' statement ELSE statement
{
    // Create blocks, generate conditional branch
    // All handled by codegen helpers
}
```

#### Loop Implementation
```c
// While loop codegen:
void codegen_while_loop(ExprValue (*cond_fn)(void),
                       void (*body_fn)(void));
```

### Phase 3: Functions & Advanced Features

#### Function Calls
```c
// Add to codegen.h:
ExprValue codegen_call(const char *func_name, 
                       ExprValue *args, 
                       int arg_count);
```

#### Pointers & Arrays
```c
ExprValue codegen_array_access(ExprValue array, ExprValue index);
ExprValue codegen_pointer_deref(ExprValue ptr);
```

### Phase 4: Preprocessor

#### Simple Implementation
```c
// Create preprocessor.h/c
typedef struct {
    char *name;
    char *value;
} Macro;

char* preprocess_file(const char *filename);
```

#### Integration
```c
// In pdriver.c, before yyparse():
char *preprocessed = preprocess_file(argv[1]);
yy_scan_string(preprocessed);
```

### Phase 5: Standard Library

#### Printf Implementation
```c
// Declare external function:
void codegen_declare_printf(void) {
    LLVMTypeRef printf_type = LLVMFunctionType(
        LLVMInt32TypeInContext(ctx),
        (LLVMTypeRef[]){LLVMPointerType(LLVMInt8TypeInContext(ctx), 0)},
        1, 1  // vararg
    );
    LLVMAddFunction(module, "printf", printf_type);
}
```

### Phase 6: Optimizations

#### Add LLVM Passes
```c
// In interpreter.c:
void interpreter_optimize(int level) {
    LLVMPassManagerRef pm = LLVMCreatePassManager();
    
    if (level >= 1) {
        LLVMAddInstructionCombiningPass(pm);
        LLVMAddReassociatePass(pm);
    }
    
    if (level >= 2) {
        LLVMAddGVNPass(pm);
        LLVMAddCFGSimplificationPass(pm);
    }
    
    if (level >= 3) {
        LLVMAddAggressiveDCEPass(pm);
        LLVMAddInlinerPass(pm);
    }
    
    LLVMRunPassManager(pm, module);
}
```

### Phase 7: Benchmarking

#### Create Benchmark Suite
```bash
#!/bin/bash
# benchmark.sh

# Test programs
TESTS="factorial fibonacci matrix_mult quicksort"

for test in $TESTS; do
    echo "Benchmarking $test..."
    
    # Compile with different compilers
    gcc -O2 tests/$test.c -o /tmp/gcc_$test
    clang -O2 tests/$test.c -o /tmp/clang_$test
    tcc tests/$test.c -o /tmp/tcc_$test
    ./cc tests/$test.c -o /tmp/our_$test
    
    # Benchmark execution
    time /tmp/gcc_$test
    time /tmp/clang_$test  
    time /tmp/tcc_$test
    time /tmp/our_$test
done
```

## File-by-File Changes

### core/parser/pparser.y
- Line 47-58: Update union to use ExprValue
- Line 1207-1256: Replace primary_expression actions
- Line 895-998: Replace arithmetic operations
- Line 784-893: Replace comparison operations
- Line 560-581: Replace statement actions
- Remove stub functions (lines 1275-1283)

### core/parser/codegen.c
- ✅ Already complete with all helpers

### core/parser/codegen.h
- ✅ Already complete with all declarations

### New Files Needed
- `core/preprocessor.h` - Preprocessor interface
- `core/preprocessor.c` - Preprocessor implementation
- `tests/benchmark.sh` - Benchmark script
- `tests/*.c` - Test programs

## Testing Strategy

### Unit Tests
```c
// test_codegen.c
void test_integer_constant() {
    ExprValue val = codegen_integer_constant(42);
    assert(LLVMIsConstant(val.value));
}

void test_binary_add() {
    ExprValue a = codegen_integer_constant(5);
    ExprValue b = codegen_integer_constant(3);
    ExprValue result = codegen_binary_op("+", a, b);
    // Verify result
}
```

### Integration Tests
```c
// test_programs/arithmetic.c
int main() {
    return 5 + 3 * 2;  // Should return 11
}

// test_programs/loops.c
int main() {
    int sum = 0;
    for (int i = 0; i < 10; i++) {
        sum += i;
    }
    return sum;  // Should return 45
}
```

## Estimated Effort

| Phase | Files Changed | Lines Modified | Commits | Time |
|-------|--------------|----------------|---------|------|
| 1. Expressions | 1 | ~400 | 2 | High Priority |
| 2. Control Flow | 2 | ~300 | 2 | High Priority |
| 3. Functions | 3 | ~400 | 3 | Medium Priority |
| 4. Preprocessor | 2 | ~500 | 2 | Medium Priority |
| 5. Stdlib | 2 | ~300 | 2 | Medium Priority |
| 6. Optimization | 1 | ~100 | 1 | Low Priority |
| 7. Benchmark | 1 | ~200 | 1 | Low Priority |
| **Total** | **12** | **~2200** | **13** | **Full Implementation** |

## Next Immediate Steps

1. ✅ Create codegen infrastructure
2. 🔄 Update parser union (next commit)
3. 🔄 Replace expression operations (next commit)
4. 📋 Replace control flow (following commit)
5. 📋 Continue through phases...

## How to Continue

For anyone continuing this work:

1. Start with Phase 1, Step 1 (update union)
2. Use find/replace for systematic changes
3. Test after each major change
4. Commit frequently with clear messages
5. Follow the patterns in codegen.c

The foundation is solid. The path forward is clear. The implementation is systematic.
