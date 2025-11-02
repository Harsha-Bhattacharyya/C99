%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "../interpreter.h"
#include "codegen.h"

// Function prototypes
extern int yylex(void);
extern int yyparse(void);
extern FILE *yyin;
extern char *yytext;
extern int yylineno;

void yyerror(const char *s);
void emit_function_start(const char *name, const char *return_type);
void emit_function_end(void);
void init_mlir_module(void);
void print_mlir_module(void);
void cleanup_mlir_module(void);

%}

%code requires {
#include "codegen.h"
}

%union {
    char *sval;
    int integer;
    double floating;
    ExprValue expr;
}

// Token declarations matching the lexer
%token <sval> IDENTIFIER STRING_LITERAL
%token <integer> INTEGER_CONSTANT
%token <floating> FLOATING_CONSTANT
%token <sval> CHARACTER_CONSTANT
%token <sval> BOOL_LITERAL

// Keywords
%token AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT DO
%token DOUBLE ELSE ENUM EXTERN FLOAT FOR GOTO IF
%token INLINE INT LONG REGISTER RESTRICT RETURN SHORT SIGNED
%token SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID
%token VOLATILE WHILE
%token _BOOL _COMPLEX _IMAGINARY
%token <sval> TYPEDEF_NAME

// Operators
%token ARROW INC_OP DEC_OP LEFT_SHIFT RIGHT_SHIFT
%token LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP
%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN
%token LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN

// Punctuators
%token ELLIPSIS HASH DOUBLE_HASH

// Expression types
%type <expr> expression primary_expression postfix_expression
%type <expr> unary_expression cast_expression multiplicative_expression
%type <expr> additive_expression shift_expression relational_expression
%type <expr> equality_expression and_expression exclusive_or_expression
%type <expr> inclusive_or_expression logical_and_expression logical_or_expression
%type <expr> conditional_expression assignment_expression constant_expression
%type <sval> type_specifier declaration_specifiers declarator
%type <sval> direct_declarator parameter_declaration

// Precedence and associativity
%right '=' MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN XOR_ASSIGN OR_ASSIGN
%right '?' ':'
%left OR_OP
%left AND_OP
%left '|'
%left '^'
%left '&'
%left EQ_OP NE_OP
%left '<' '>' LE_OP GE_OP
%left LEFT_SHIFT RIGHT_SHIFT
%left '+' '-'
%left '*' '/' '%'
%right SIZEOF '!' '~' INC_OP DEC_OP UNARY_MINUS UNARY_PLUS
%left '[' ']' '(' ')' '.' ARROW

%precedence THEN
%precedence ELSE

%start translation_unit

%%

translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;

external_declaration
    : function_definition
    | declaration
    ;

function_definition
    : declaration_specifiers declarator 
    {
        emit_function_start($2, $1);
    }
    compound_statement
    {
        emit_function_end();
        free($1);
        free($2);
    }
    | declarator
    {
        emit_function_start($1, "i32");  // Default return type
    }
    compound_statement
    {
        emit_function_end();
        free($1);
    }
    ;

declaration
    : declaration_specifiers ';'
    {
        free($1);
    }
    | declaration_specifiers init_declarator_list ';'
    {
        free($1);
    }
    ;

declaration_specifiers
    : storage_class_specifier
    {
        $$ = strdup("int");  // Default type
    }
    | type_specifier
    {
        $$ = $1;
    }
    | type_qualifier
    {
        $$ = strdup("int");  // Default type
    }
    | function_specifier
    {
        $$ = strdup("int");  // Default type
    }
    | declaration_specifiers storage_class_specifier
    {
        $$ = $1; // Keep existing type
    }
    | declaration_specifiers type_specifier
    {
        // A real implementation would combine e.g. long & int.
        // For now, just keep the first type found.
        free($2);
        $$ = $1;
    }
    | declaration_specifiers type_qualifier
    {
        $$ = $1; // Keep existing type
    }
    | declaration_specifiers function_specifier
    {
        $$ = $1; // Keep existing type
    }
    ;

init_declarator_list
    : init_declarator
    | init_declarator_list ',' init_declarator
    ;

init_declarator
    : declarator
    {
        // Create variable allocation
        if (codegen_ctx && codegen_ctx->current_function) {
            LLVMValueRef alloca = codegen_create_variable($1, get_c_type("i32"));
        }
        free($1);
    }
    | declarator '=' initializer
    {
        // Create variable with initialization (TODO: use initializer value)
        if (codegen_ctx && codegen_ctx->current_function) {
            LLVMValueRef alloca = codegen_create_variable($1, get_c_type("i32"));
        }
        free($1);
    }
    ;

storage_class_specifier
    : TYPEDEF | EXTERN | STATIC | AUTO | REGISTER
    ;

type_specifier
    : VOID      { $$ = strdup("void"); }
    | CHAR      { $$ = strdup("i8"); }
    | SHORT     { $$ = strdup("i16"); }
    | INT       { $$ = strdup("i32"); }
    | LONG      { $$ = strdup("i64"); }
    | FLOAT     { $$ = strdup("f32"); }
    | DOUBLE    { $$ = strdup("f64"); }
    | SIGNED    { $$ = strdup("i32"); }
    | UNSIGNED  { $$ = strdup("i32"); }
    | _BOOL     { $$ = strdup("i1"); }
    | struct_or_union_specifier { $$ = strdup("ptr"); }
    | enum_specifier { $$ = strdup("i32"); }
    | TYPEDEF_NAME { $$ = strdup("i32"); }
    ;

struct_or_union_specifier
    : struct_or_union IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER
    ;

struct_or_union
    : STRUCT
    | UNION
    ;

struct_declaration_list
    : struct_declaration
    | struct_declaration_list struct_declaration
    ;

struct_declaration
    : specifier_qualifier_list struct_declarator_list ';'
    ;

specifier_qualifier_list
    : type_specifier
    | type_qualifier
    | specifier_qualifier_list type_specifier
    | specifier_qualifier_list type_qualifier
    ;

struct_declarator_list
    : struct_declarator
    | struct_declarator_list ',' struct_declarator
    ;

struct_declarator
    : declarator
    | ':' constant_expression
    | declarator ':' constant_expression
    ;

enum_specifier
    : ENUM '{' enumerator_list '}'
    | ENUM IDENTIFIER '{' enumerator_list '}'
    | ENUM '{' enumerator_list ',' '}'
    | ENUM IDENTIFIER '{' enumerator_list ',' '}'
    | ENUM IDENTIFIER
    ;

enumerator_list
    : enumerator
    | enumerator_list ',' enumerator
    ;

enumerator
    : IDENTIFIER
    | IDENTIFIER '=' constant_expression
    ;

type_qualifier
    : CONST | RESTRICT | VOLATILE
    ;

function_specifier
    : INLINE
    ;

declarator
    : pointer direct_declarator
    {
        $$ = $2;
    }
    | direct_declarator
    {
        $$ = $1;
    }
    ;

direct_declarator
    : IDENTIFIER
    {
        $$ = strdup($1);
    }
    | '(' declarator ')'
    {
        $$ = $2;
    }
    | direct_declarator '[' constant_expression ']'
    {
        $$ = $1;
    }
    | direct_declarator '[' ']'
    {
        $$ = $1;
    }
    | direct_declarator '(' parameter_type_list ')'
    {
        $$ = $1;
    }
    | direct_declarator '(' identifier_list ')'
    {
        $$ = $1;
    }
    | direct_declarator '(' ')'
    {
        $$ = $1;
    }
    ;

pointer
    : '*'
    | '*' type_qualifier_list
    | '*' pointer
    | '*' type_qualifier_list pointer
    ;

type_qualifier_list
    : type_qualifier
    | type_qualifier_list type_qualifier
    ;

parameter_type_list
    : parameter_list
    | parameter_list ',' ELLIPSIS
    ;

parameter_list
    : parameter_declaration
    | parameter_list ',' parameter_declaration
    ;

parameter_declaration
    : declaration_specifiers declarator
    {
        $$ = $2;
        free($1);
    }
    | declaration_specifiers abstract_declarator
    {
        $$ = strdup("param");
        free($1);
    }
    | declaration_specifiers
    {
        $$ = strdup("param");
        free($1);
    }
    ;

identifier_list
    : IDENTIFIER
    | identifier_list ',' IDENTIFIER
    ;

type_name
    : specifier_qualifier_list
    | specifier_qualifier_list abstract_declarator
    ;

abstract_declarator
    : pointer
    | direct_abstract_declarator
    | pointer direct_abstract_declarator
    ;

direct_abstract_declarator
    : '(' abstract_declarator ')'
    | '[' ']'
    | '[' constant_expression ']'
    | direct_abstract_declarator '[' ']'
    | direct_abstract_declarator '[' constant_expression ']'
    | '(' ')'
    | '(' parameter_type_list ')'
    | direct_abstract_declarator '(' ')'
    | direct_abstract_declarator '(' parameter_type_list ')'
    ;

initializer
    : assignment_expression
    | '{' initializer_list '}'
    | '{' initializer_list ',' '}'
    ;

initializer_list
    : initializer
    | designation initializer
    | initializer_list ',' initializer
    | initializer_list ',' designation initializer
    ;

designation
    : designator_list '='
    ;

designator_list
    : designator
    | designator_list designator
    ;

designator
    : '[' constant_expression ']'
    | '.' IDENTIFIER
    ;

statement
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

labeled_statement
    : IDENTIFIER ':' statement
    {
        // TODO: Implement labeled statement in Phase 2
        free($1);
    }
    | CASE constant_expression ':' statement
    {
        // TODO: Implement case statement in Phase 2
    }
    | DEFAULT ':' statement
    {
        // TODO: Implement default statement in Phase 2
    }
    ;

compound_statement
    : '{' '}'
    | '{' block_item_list '}'
    ;

block_item_list
    : block_item
    | block_item_list block_item
    ;

block_item
    : declaration
    | statement
    ;

expression_statement
    : ';'
    | expression ';'
    {
        if ($1.value) {
            free($1.value);
        }
        if ($1.type) {
            free($1.type);
        }
    }
    ;

selection_statement
    : IF '(' expression ')' statement %prec THEN
    {
        // TODO: Implement if statement code generation in Phase 2
    }
    | IF '(' expression ')' statement ELSE statement
    {
        // TODO: Implement if/else statement code generation in Phase 2
    }
    | SWITCH '(' expression ')' statement
    {
        // TODO: Implement switch statement code generation in Phase 2
    }
    ;

iteration_statement
    : WHILE '(' expression ')' statement
    {
        // TODO: Implement while loop code generation in Phase 2
    }
    | DO statement WHILE '(' expression ')' ';'
    {
        // TODO: Implement do-while loop code generation in Phase 2
    }
    | FOR '(' expression_statement expression_statement ')' statement
    {
        // TODO: Implement for loop code generation in Phase 2
    }
    | FOR '(' expression_statement expression_statement expression ')' statement
    {
        // TODO: Implement for loop code generation in Phase 2
    }
    | FOR '(' declaration expression_statement ')' statement
    {
        // TODO: Implement for loop with declaration code generation in Phase 2
    }
    | FOR '(' declaration expression_statement expression ')' statement
    {
        // TODO: Implement for loop with declaration code generation in Phase 2
    }
    ;

jump_statement
    : GOTO IDENTIFIER ';'
    {
        // GOTO - would need label management for full implementation
        free($2);
    }
    | CONTINUE ';'
    {
        // CONTINUE - would need loop block tracking for full implementation
    }
    | BREAK ';'
    {
        // BREAK - would need loop/switch block tracking for full implementation
    }
    | RETURN ';'
    {
        codegen_return_void();
    }
    | RETURN expression ';'
    {
        codegen_return($2);
    }
    ;

expression
    : assignment_expression
    {
        $$ = $1;
    }
    | expression ',' assignment_expression
    {
        // Comma operator - evaluate both, return right operand
        $$ = $3;
    }
    ;

assignment_expression
    : conditional_expression
    {
        $$ = $1;
    }
    | unary_expression assignment_operator assignment_expression
    {
        // TODO: Implement assignment operators in Phase 2
        $$ = $3;
    }
    ;

assignment_operator
    : '=' | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN | ADD_ASSIGN | SUB_ASSIGN
    | LEFT_ASSIGN | RIGHT_ASSIGN | AND_ASSIGN | XOR_ASSIGN | OR_ASSIGN
    ;

conditional_expression
    : logical_or_expression
    {
        $$ = $1;
    }
    | logical_or_expression '?' expression ':' conditional_expression
    {
        // TODO: Implement ternary operator in Phase 2
        $$ = $1;
    }
    ;

constant_expression
    : conditional_expression
    {
        $$ = $1;
    }
    ;

logical_or_expression
    : logical_and_expression
    {
        $$ = $1;
    }
    | logical_or_expression OR_OP logical_and_expression
    {
        // TODO: Implement logical OR in Phase 2
        $$ = $1;
    }
    ;

logical_and_expression
    : inclusive_or_expression
    {
        $$ = $1;
    }
    | logical_and_expression AND_OP inclusive_or_expression
    {
        // TODO: Implement logical AND in Phase 2
        $$ = $1;
    }
    ;

inclusive_or_expression
    : exclusive_or_expression
    {
        $$ = $1;
    }
    | inclusive_or_expression '|' exclusive_or_expression
    {
        $$ = codegen_binary_op("|", $1, $3);
    }
    ;

exclusive_or_expression
    : and_expression
    {
        $$ = $1;
    }
    | exclusive_or_expression '^' and_expression
    {
        $$ = codegen_binary_op("^", $1, $3);
    }
    ;

and_expression
    : equality_expression
    {
        $$ = $1;
    }
    | and_expression '&' equality_expression
    {
        $$ = codegen_binary_op("&", $1, $3);
    }
    ;

equality_expression
    : relational_expression
    {
        $$ = $1;
    }
    | equality_expression EQ_OP relational_expression
    {
        $$ = codegen_binary_op("==", $1, $3);
    }
    | equality_expression NE_OP relational_expression
    {
        $$ = codegen_binary_op("!=", $1, $3);
    }
    ;

relational_expression
    : shift_expression
    {
        $$ = $1;
    }
    | relational_expression '<' shift_expression
    {
        $$ = codegen_binary_op("<", $1, $3);
    }
    | relational_expression '>' shift_expression
    {
        $$ = codegen_binary_op(">", $1, $3);
    }
    | relational_expression LE_OP shift_expression
    {
        $$ = codegen_binary_op("<=", $1, $3);
    }
    | relational_expression GE_OP shift_expression
    {
        $$ = codegen_binary_op(">=", $1, $3);
    }
    ;

shift_expression
    : additive_expression
    {
        $$ = $1;
    }
    | shift_expression LEFT_SHIFT additive_expression
    {
        $$ = codegen_binary_op("<<", $1, $3);
    }
    | shift_expression RIGHT_SHIFT additive_expression
    {
        $$ = codegen_binary_op(">>", $1, $3);
    }
    ;

additive_expression
    : multiplicative_expression
    {
        $$ = $1;
    }
    | additive_expression '+' multiplicative_expression
    {
        $$ = codegen_binary_op("+", $1, $3);
    }
    | additive_expression '-' multiplicative_expression
    {
        $$ = codegen_binary_op("-", $1, $3);
    }
    ;

multiplicative_expression
    : cast_expression
    {
        $$ = $1;
    }
    | multiplicative_expression '*' cast_expression
    {
        $$ = codegen_binary_op("*", $1, $3);
    }
    | multiplicative_expression '/' cast_expression
    {
        $$ = codegen_binary_op("/", $1, $3);
    }
    | multiplicative_expression '%' cast_expression
    {
        $$ = codegen_binary_op("%", $1, $3);
    }
    ;

cast_expression
    : unary_expression
    {
        $$ = $1;
    }
    | '(' type_name ')' cast_expression
    {
        // Type casting - for now, just pass through the value
        // In a more complete implementation, we'd emit cast operations
        $$ = $4;
    }
    ;

unary_expression
    : postfix_expression
    {
        $$ = $1;
    }
    | INC_OP unary_expression
    {
        // TODO: Implement increment operator in Phase 2
        $$ = $2;
    }
    | DEC_OP unary_expression
    {
        // TODO: Implement decrement operator in Phase 2
        $$ = $2;
    }
    | unary_operator cast_expression
    {
        // TODO: Implement unary operators in Phase 2
        $$ = $2;
    }
    | SIZEOF unary_expression
    {
        // TODO: Implement sizeof operator in Phase 2
        $$ = codegen_integer_constant(4);  // Simplified sizeof returns 4
    }
    | SIZEOF '(' type_name ')'
    {
        // TODO: Implement sizeof operator in Phase 2
        $$ = codegen_integer_constant(4);  // Simplified sizeof returns 4
    }
    ;

unary_operator
    : '&' | '*' | '+' | '-' | '~' | '!'
    ;

postfix_expression
    : primary_expression
    {
        $$ = $1;
    }
    | postfix_expression '[' expression ']'
    {
        // TODO: Implement array access in Phase 3
        $$ = $1;
    }
    | postfix_expression '(' ')'
    {
        // TODO: Implement function calls in Phase 3
        $$ = codegen_integer_constant(0);
    }
    | postfix_expression '(' argument_expression_list ')'
    {
        // TODO: Implement function calls with arguments in Phase 3
        $$ = codegen_integer_constant(0);
    }
    | postfix_expression '.' IDENTIFIER
    {
        // TODO: Implement struct member access in Phase 3
        free($3);
        $$ = $1;
    }
    | postfix_expression ARROW IDENTIFIER
    {
        // TODO: Implement pointer member access in Phase 3
        free($3);
        $$ = $1;
    }
    | postfix_expression INC_OP
    {
        // TODO: Implement post-increment in Phase 2
        $$ = $1;
    }
    | postfix_expression DEC_OP
    {
        // TODO: Implement post-decrement in Phase 2
        $$ = $1;
    }
    | '(' type_name ')' '{' initializer_list '}'
    {
        // TODO: Implement compound literals in Phase 3
        $$ = codegen_integer_constant(0);
    }
    | '(' type_name ')' '{' initializer_list ',' '}'
    {
        // TODO: Implement compound literals in Phase 3
        $$ = codegen_integer_constant(0);
    }
    ;

argument_expression_list
    : assignment_expression
    {
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
    }
    | argument_expression_list ',' assignment_expression
    {
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

primary_expression
    : IDENTIFIER
    {
        $$ = codegen_load_variable($1);
        free($1);
    }
    | INTEGER_CONSTANT
    {
        $$ = codegen_integer_constant($1);
    }
    | FLOATING_CONSTANT
    {
        $$ = codegen_float_constant($1);
    }
    | CHARACTER_CONSTANT
    {
        int char_val = ($1 && $1[0]) ? (int)$1[0] : 0;
        ExprValue result;
        result.value = LLVMConstInt(LLVMInt8TypeInContext(get_llvm_context()), char_val, 0);
        result.type = LLVMInt8TypeInContext(get_llvm_context());
        free($1);
        $$ = result;
    }
    | STRING_LITERAL
    {
        // String literals - for now return null pointer
        ExprValue result;
        result.value = LLVMConstPointerNull(LLVMPointerType(LLVMInt8TypeInContext(get_llvm_context()), 0));
        result.type = LLVMPointerType(LLVMInt8TypeInContext(get_llvm_context()), 0);
        free($1);
        $$ = result;
    }
    | '(' expression ')'
    {
        $$ = $2;
    }
    ;

%%

// Implementation of helper functions
void yyerror(const char *s) {
    fprintf(stderr, "Parse error at line %d: %s\n", yylineno, s);
}

void emit_function_start(const char *name, const char *return_type) {
    LLVMTypeRef ret_type = get_c_type(return_type);
    codegen_function_start(name, ret_type, NULL, 0);
}

void emit_function_end(void) {
    codegen_function_end();
}

// Wrapper functions to maintain compatibility with pdriver.c
void init_mlir_module(void) {
    // Handled by codegen_init in pdriver
}

void print_mlir_module(void) {
    // Handled by interpreter_execute in pdriver
}

void cleanup_mlir_module(void) {
    // Handled by codegen_cleanup in pdriver
}
