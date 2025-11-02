%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "../interpreter.h"

// Global state for LLVM code generation
static LLVMValueRef current_llvm_function = NULL;
static LLVMBasicBlockRef current_llvm_block = NULL;
int temp_counter = 0;
int block_counter = 0;

// Symbol table for variables
typedef struct symbol {
    char *name;
    char *type;
    LLVMValueRef llvm_value;
    struct symbol *next;
} symbol_t;

symbol_t *symbol_table = NULL;

// Function prototypes
extern int yylex(void);
extern int yyparse(void);
extern FILE *yyin;
extern char *yytext;
extern int yylineno;

void yyerror(const char *s);
char *gen_temp(void);
char *gen_block_label(void);
void emit_operation(const char *format, ...);  // Stub for compatibility
void emit_block_start(const char *label);      // Stub for compatibility
void emit_function_start(const char *name, const char *return_type);
void emit_function_end(void);
void add_symbol(const char *name, const char *type);
symbol_t *lookup_symbol(const char *name);
LLVMTypeRef c_type_to_llvm(const char *c_type);
void init_mlir_module(void);
void print_mlir_module(void);
void cleanup_mlir_module(void);

%}

%union {
    char *sval;
    int integer;
    double floating;
    struct {
        char *value;
        char *type;
    } expr;
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
    : declaration_specifiers declarator compound_statement
    {
        emit_function_start($2, $1);
        emit_function_end();
        free($1);
        free($2);
    }
    | declarator compound_statement
    {
        emit_function_start($1, "i32");  // Default return type
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
        // Add variable declaration
        add_symbol($1, "i32");
        free($1);
    }
    | declarator '=' initializer
    {
        add_symbol($1, "i32");
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
        emit_operation("^%s:", $1);
        free($1);
    }
    | CASE constant_expression ':' statement
    | DEFAULT ':' statement
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
        char *then_label = gen_block_label();
        char *end_label = gen_block_label();
        
        emit_operation("  cf.cond_br %s, ^%s, ^%s", $3.value, then_label, end_label);
        emit_block_start(then_label);
        emit_operation("  cf.br ^%s", end_label);
        emit_block_start(end_label);
        
        free($3.value);
        free($3.type);
        free(then_label);
        free(end_label);
    }
    | IF '(' expression ')' statement ELSE statement
    {
        char *then_label = gen_block_label();
        char *else_label = gen_block_label();
        char *end_label = gen_block_label();
        
        emit_operation("  cf.cond_br %s, ^%s, ^%s", $3.value, then_label, else_label);
        emit_block_start(then_label);
        emit_operation("  cf.br ^%s", end_label);
        emit_block_start(else_label);
        emit_operation("  cf.br ^%s", end_label);
        emit_block_start(end_label);
        
        free($3.value);
        free($3.type);
        free(then_label);
        free(else_label);
        free(end_label);
    }
    | SWITCH '(' expression ')' statement
    {
        free($3.value);
        free($3.type);
    }
    ;

iteration_statement
    : WHILE '(' expression ')' statement
    {
        char *loop_label = gen_block_label();
        char *body_label = gen_block_label();
        char *end_label = gen_block_label();
        
        emit_operation("  cf.br ^%s", loop_label);
        emit_block_start(loop_label);
        emit_operation("  cf.cond_br %s, ^%s, ^%s", $3.value, body_label, end_label);
        emit_block_start(body_label);
        emit_operation("  cf.br ^%s", loop_label);
        emit_block_start(end_label);
        
        free($3.value);
        free($3.type);
        free(loop_label);
        free(body_label);
        free(end_label);
    }
    | DO statement WHILE '(' expression ')' ';'
    {
        free($5.value);
        free($5.type);
    }
    | FOR '(' expression_statement expression_statement ')' statement
    | FOR '(' expression_statement expression_statement expression ')' statement
    {
        free($5.value);
        free($5.type);
    }
    | FOR '(' declaration expression_statement ')' statement
    | FOR '(' declaration expression_statement expression ')' statement
    {
        free($5.value);
        free($5.type);
    }
    ;

jump_statement
    : GOTO IDENTIFIER ';'
    {
        emit_operation("  cf.br ^%s", $2);
        free($2);
    }
    | CONTINUE ';'
    {
        emit_operation("  cf.br ^continue");
    }
    | BREAK ';'
    {
        emit_operation("  cf.br ^break");
    }
    | RETURN ';'
    {
        emit_operation("  return");
    }
    | RETURN expression ';'
    {
        emit_operation("  return %s : %s", $2.value, $2.type);
        free($2.value);
        free($2.type);
    }
    ;

expression
    : assignment_expression
    {
        $$ = $1;
    }
    | expression ',' assignment_expression
    {
        // Use the right operand as the result
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
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
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  store %s, %s : memref<%s>", $3.value, $1.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
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
        char *temp = gen_temp();
        char *mlir_type = $3.type ? strdup($3.type) : strdup("i32");
        
        emit_operation("  %s = select %s, %s, %s : %s", temp, $1.value, $3.value, $5.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
        if ($5.value) free($5.value);
        if ($5.type) free($5.type);
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
        char *temp = gen_temp();
        emit_operation("  %s = arith.ori %s, %s : i1", temp, $1.value, $3.value);
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

logical_and_expression
    : inclusive_or_expression
    {
        $$ = $1;
    }
    | logical_and_expression AND_OP inclusive_or_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.andi %s, %s : i1", temp, $1.value, $3.value);
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

inclusive_or_expression
    : exclusive_or_expression
    {
        $$ = $1;
    }
    | inclusive_or_expression '|' exclusive_or_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = arith.ori %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

exclusive_or_expression
    : and_expression
    {
        $$ = $1;
    }
    | exclusive_or_expression '^' and_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = arith.xori %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

and_expression
    : equality_expression
    {
        $$ = $1;
    }
    | and_expression '&' equality_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = arith.andi %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

equality_expression
    : relational_expression
    {
        $$ = $1;
    }
    | equality_expression EQ_OP relational_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.cmpi eq, %s, %s : %s", temp, $1.value, $3.value, $1.type ? $1.type : "i32");
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | equality_expression NE_OP relational_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.cmpi ne, %s, %s : %s", temp, $1.value, $3.value, $1.type ? $1.type : "i32");
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

relational_expression
    : shift_expression
    {
        $$ = $1;
    }
    | relational_expression '<' shift_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.cmpi slt, %s, %s : %s", temp, $1.value, $3.value, $1.type ? $1.type : "i32");
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | relational_expression '>' shift_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.cmpi sgt, %s, %s : %s", temp, $1.value, $3.value, $1.type ? $1.type : "i32");
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | relational_expression LE_OP shift_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.cmpi sle, %s, %s : %s", temp, $1.value, $3.value, $1.type ? $1.type : "i32");
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | relational_expression GE_OP shift_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.cmpi sge, %s, %s : %s", temp, $1.value, $3.value, $1.type ? $1.type : "i32");
        
        $$.value = temp;
        $$.type = strdup("i1");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

shift_expression
    : additive_expression
    {
        $$ = $1;
    }
    | shift_expression LEFT_SHIFT additive_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = arith.shli %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | shift_expression RIGHT_SHIFT additive_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = arith.shrsi %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

additive_expression
    : multiplicative_expression
    {
        $$ = $1;
    }
    | additive_expression '+' multiplicative_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        if (strstr(mlir_type, "f")) {
            emit_operation("  %s = arith.addf %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        } else {
            emit_operation("  %s = arith.addi %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        }
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    |
    additive_expression '-' multiplicative_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        if (strstr(mlir_type, "f")) {
            emit_operation("  %s = arith.subf %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        } else {
          emit_operation("  %s = arith.subi %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        }

        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    ;

multiplicative_expression
    : cast_expression
    {
        $$ = $1;
    }
    | multiplicative_expression '*' cast_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        if (strstr(mlir_type, "f")) {
            emit_operation("  %s = arith.mulf %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        } else {
            emit_operation("  %s = arith.muli %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        }
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | multiplicative_expression '/' cast_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        if (strstr(mlir_type, "f")) {
            emit_operation("  %s = arith.divf %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        } else {
            emit_operation("  %s = arith.divsi %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        }
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | multiplicative_expression '%' cast_expression
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = arith.remsi %s, %s : %s", temp, $1.value, $3.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
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
        char *temp = gen_temp();
        char *one_temp = gen_temp();
        char *mlir_type = $2.type ? strdup($2.type) : strdup("i32");
        
        emit_operation("  %s = arith.constant 1 : %s", one_temp, mlir_type);
        emit_operation("  %s = arith.addi %s, %s : %s", temp, $2.value, one_temp, mlir_type);
        emit_operation("  store %s, %s : memref<%s>", temp, $2.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($2.value) free($2.value);
        if ($2.type) free($2.type);
    }
    | DEC_OP unary_expression
    {
        char *temp = gen_temp();
        char *one_temp = gen_temp();
        char *mlir_type = $2.type ? strdup($2.type) : strdup("i32");
        
        emit_operation("  %s = arith.constant 1 : %s", one_temp, mlir_type);
        emit_operation("  %s = arith.subi %s, %s : %s", temp, $2.value, one_temp, mlir_type);
        emit_operation("  store %s, %s : memref<%s>", temp, $2.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($2.value) free($2.value);
        if ($2.type) free($2.type);
    }
    | unary_operator cast_expression
    {
        $$ = $2; // For now, just pass through
    }
    | SIZEOF unary_expression
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.constant 4 : i32", temp); // Simplified sizeof
        
        $$.value = temp;
        $$.type = strdup("i32");
        
        if ($2.value) free($2.value);
        if ($2.type) free($2.type);
    }
    | SIZEOF '(' type_name ')'
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.constant 4 : i32", temp); // Simplified sizeof
        
        $$.value = temp;
        $$.type = strdup("i32");
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
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = memref.load %s[%s] : memref<?x%s>", temp, $1.value, $3.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        if ($3.value) free($3.value);
        if ($3.type) free($3.type);
    }
    | postfix_expression '(' ')'
    {
        char *temp = gen_temp();
        emit_operation("  %s = call @%s() : () -> i32", temp, $1.value);
        
        $$.value = temp;
        $$.type = strdup("i32");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
    }
    | postfix_expression '(' argument_expression_list ')'
    {
        char *temp = gen_temp();
        emit_operation("  %s = call @%s() : () -> i32", temp, $1.value); // Simplified
        
        $$.value = temp;
        $$.type = strdup("i32");
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
    }
    | postfix_expression '.' IDENTIFIER
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = memref.load %s : memref<%s>", temp, $1.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        free($3);
    }
    | postfix_expression ARROW IDENTIFIER
    {
        char *temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = memref.load %s : memref<%s>", temp, $1.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
        free($3);
    }
    | postfix_expression INC_OP
    {
        char *temp = gen_temp();
        char *one_temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = memref.load %s : memref<%s>", temp, $1.value, mlir_type);
        emit_operation("  %s = arith.constant 1 : %s", one_temp, mlir_type);
        emit_operation("  %s = arith.addi %s, %s : %s", temp, temp, one_temp, mlir_type);
        emit_operation("  store %s, %s : memref<%s>", temp, $1.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
    }
    | postfix_expression DEC_OP
    {
        char *temp = gen_temp();
        char *one_temp = gen_temp();
        char *mlir_type = $1.type ? strdup($1.type) : strdup("i32");
        
        emit_operation("  %s = memref.load %s : memref<%s>", temp, $1.value, mlir_type);
        emit_operation("  %s = arith.constant 1 : %s", one_temp, mlir_type);
        emit_operation("  %s = arith.subi %s, %s : %s", temp, temp, one_temp, mlir_type);
        emit_operation("  store %s, %s : memref<%s>", temp, $1.value, mlir_type);
        
        $$.value = temp;
        $$.type = mlir_type;
        
        if ($1.value) free($1.value);
        if ($1.type) free($1.type);
    }
    | '(' type_name ')' '{' initializer_list '}'
    {
        char *temp = gen_temp();
        $$.value = temp;
        $$.type = strdup("i32");
    }
    | '(' type_name ')' '{' initializer_list ',' '}'
    {
        char *temp = gen_temp();
        $$.value = temp;
        $$.type = strdup("i32");
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
        symbol_t *sym = lookup_symbol($1);
        if (sym) {
            char *temp = gen_temp();
            emit_operation("  %s = memref.load %%%s : memref<%s>", temp, sym->name, sym->type);
            $$.value = temp;
            $$.type = strdup(sym->type);
        } else {
            $$.value = strdup($1);
            $$.type = strdup("i32");
        }
        free($1);
    }
    | INTEGER_CONSTANT
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.constant %d : i32", temp, $1);
        $$.value = temp;
        $$.type = strdup("i32");
    }
    | FLOATING_CONSTANT
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.constant %f : f64", temp, $1);
        $$.value = temp;
        $$.type = strdup("f64");
    }
    | CHARACTER_CONSTANT
    {
        char *temp = gen_temp();
        emit_operation("  %s = arith.constant %s : i8", temp, $1);
        $$.value = temp;
        $$.type = strdup("i8");
        free($1);
    }
    | STRING_LITERAL
    {
        char *temp = gen_temp();
        emit_operation("  %s = memref.get_global @str_literal : memref<?xi8>", temp);
        $$.value = temp;
        $$.type = strdup("memref<?xi8>");
        free($1);
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

char *gen_temp(void) {
    char *temp = malloc(16);
    snprintf(temp, 16, "t%d", temp_counter++);
    return temp;
}

char *gen_block_label(void) {
    char *label = malloc(16);
    snprintf(label, 16, "bb%d", block_counter++);
    return label;
}

// Stub functions for compatibility with old MLIR code
void emit_operation(const char *format, ...) {
    // These operations are no longer needed for LLVM
    // Left as stubs for compatibility
}

void emit_block_start(const char *label) {
    // Block creation is now handled differently in LLVM
    // Left as stub for compatibility
}

void emit_function_start(const char *name, const char *return_type) {
    // Convert return type to LLVM type
    LLVMTypeRef ret_type = c_type_to_llvm(return_type);
    
    // Create function with no parameters for now
    current_llvm_function = gen_function(name, ret_type, NULL, 0);
    
    // Create entry block
    current_llvm_block = gen_basic_block(current_llvm_function, "entry");
    LLVMPositionBuilderAtEnd(get_llvm_builder(), current_llvm_block);
}

void emit_function_end(void) {
    // If the current block doesn't have a terminator, add a return
    if (current_llvm_block && !LLVMGetBasicBlockTerminator(current_llvm_block)) {
        LLVMTypeRef func_type = LLVMGlobalGetValueType(current_llvm_function);
        LLVMTypeRef ret_type = LLVMGetReturnType(func_type);
        if (LLVMGetTypeKind(ret_type) == LLVMVoidTypeKind) {
            gen_return_void();
        } else {
            // Return 0 as default
            gen_return(gen_int_constant(0, 32));
        }
    }
    current_llvm_function = NULL;
    current_llvm_block = NULL;
}

void add_symbol(const char *name, const char *type) {
    symbol_t *sym = malloc(sizeof(symbol_t));
    sym->name = strdup(name);
    sym->type = strdup(type);
    sym->llvm_value = NULL;
    sym->next = symbol_table;
    symbol_table = sym;
}

symbol_t *lookup_symbol(const char *name) {
    symbol_t *sym = symbol_table;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            return sym;
        }
        sym = sym->next;
    }
    return NULL;
}

LLVMTypeRef c_type_to_llvm(const char *c_type) {
    LLVMContextRef ctx = get_llvm_context();
    if (strcmp(c_type, "void") == 0 || strcmp(c_type, "()") == 0) 
        return LLVMVoidTypeInContext(ctx);
    if (strcmp(c_type, "i8") == 0 || strcmp(c_type, "char") == 0) 
        return LLVMInt8TypeInContext(ctx);
    if (strcmp(c_type, "i16") == 0 || strcmp(c_type, "short") == 0) 
        return LLVMInt16TypeInContext(ctx);
    if (strcmp(c_type, "i32") == 0 || strcmp(c_type, "int") == 0) 
        return LLVMInt32TypeInContext(ctx);
    if (strcmp(c_type, "i64") == 0 || strcmp(c_type, "long") == 0) 
        return LLVMInt64TypeInContext(ctx);
    if (strcmp(c_type, "f32") == 0 || strcmp(c_type, "float") == 0) 
        return LLVMFloatTypeInContext(ctx);
    if (strcmp(c_type, "f64") == 0 || strcmp(c_type, "double") == 0) 
        return LLVMDoubleTypeInContext(ctx);
    if (strcmp(c_type, "i1") == 0 || strcmp(c_type, "_Bool") == 0) 
        return LLVMInt1TypeInContext(ctx);
    return LLVMInt32TypeInContext(ctx); // Default to i32
}

// Wrapper functions to maintain compatibility with pdriver.c
void init_mlir_module(void) {
    // Just call interpreter_init - pdriver already does this
    // This function is kept for compatibility but does nothing
}

void print_mlir_module(void) {
    // Just call interpreter_execute - pdriver already does this
    // This function is kept for compatibility but does nothing  
}

void cleanup_mlir_module(void) {
    // Cleanup symbol table
    symbol_t *sym = symbol_table;
    while (sym) {
        symbol_t *next = sym->next;
        free(sym->name);
        free(sym->type);
        free(sym);
        sym = next;
    }
    symbol_table = NULL;
    
    // interpreter_cleanup is called by pdriver
}
