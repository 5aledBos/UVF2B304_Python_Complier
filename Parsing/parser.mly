(* Literals *)
%token <bool> BOOLEANLIT
%token <float> FLOATLIT
%token <int> INTEGERLIT
%token <string> STRINGLIT
%token <string> COMMENTLINE
%token <string> KWARGS
%token <string> ARGS
%token <string> STRINGLONG

(* Infix Operators*)
%token PLUS MINUS TIMES
%token DIV AND OR XOR
%token MOD INF SUP
%token ISEQUAL ISNOTEQUAL
%token SUPEQUAL INFEQUAL
%token CONDOR CONDAND COND
%token LSHIFT RSHIFT USHIFT
%token NONE AS ASYNC AWAIT DEF DEL ELIF GLOBAL NONLOCAL PASS RAISE LAMBDA IS YIELD EXCEPT FROM IN NOT WITH POWER
%token FLOORDIV BITEAND BITEOR BITEXOR BITENEGATION FUNCMETADATA FLOORDIVEQUAL AROBASQUAL POWEREQUAL DOLLAR
%token QUESTIONMARK SHARP DOUBLEQUOTE SIMPLEQUOTE BACKSLASH
(* Prefix Operators*)
%token INCR DECR EXCL TILDE
(* Assignment Operators*)
%token EQUAL PLUSEQUAL MINUSEQUAL TIMESEQUAL
%token DIVEQUAL ANDEQUAL OREQUAL XOREQUAL
%token MODEQUAL LSHIFTEQUAL RSHIFTEQUAL USHIFTEQUAL
(* Delimitors*)
%token POINT SEMICOLON COMMA COLON LBRACE
%token RBRACE LPAREN RPAREN LBRACK RBRACK
(* Modifiers*)
%token ABSTRACT PRIVATE PROTECTED PUBLIC
%token STATIC STRICTFP SYNCHRONIZED
%token VOLATILE TRANSIENT NATIVE FINAL
(* Basic Types*)
%token BYTE SHORT CHAR INT
%token LONG FLOAT DOUBLE BOOLEAN
(* Other Keywords*)
%token ASSERT BREAK CASE CATCH CLASS
%token CONTINUE DO DEFAULT ELSE ENUM
%token FINALLY FOR IF IMPLEMENTS
%token INSTANCEOF INTERFACE NEW PACKAGE
%token SUPER SWITCH THIS THROW RETURN EXTENDS
%token AROBAS THROWS TRY VOID WHILE IMPORT
(* Special Tokens *)
%token EOF
%token NEWLINE
%token TAB

%token <string> IDENT

%start prog
%type <unit> prog 
%%

prog:
	| single_input EOF {}
	
single_input:
	| NEWLINE simple_stmt compound_stmt NEWLINE {}
	
stmt:
	| simple_stmt {}
	| compound_stmt {}
	
simple_stmt:
	| small_stmt small_stmt_opt SEMICOLON NEWLINE {}
	
small_stmt_opt:
	| {}   
	| small_stmt_opt {}
	| SEMICOLON small_stmt NEWLINE {}
	
small_stmt :
	| expr_stmt
	| del_stmt {}
	| pass_stmt {}
	| flow_stmt {}
	| import_stmt {}
	| global_stmt {}
	| nonlocal_stmt {}
	| assert_stmt {}

del_stmt:
	| DEL exprlist {}

pass_stmt:
	| PASS {}
	
flow_stmt:
	| break_stmt {}
	| continue_stmt {}
	| return_stmt {}
	| raise_stmt {}
	| yield_stmt {}

break_stmt:
	| BREAK {}

continue_stmt:
	| CONTINUE {}
	
return_stmt:
	| RETURN testlist {}
	
yield_stmt:
	| yield_expr {}
	
raise_stmt:
	| RAISE test FROM test
	
import_stmt:
	| import_name
	| import_from
	
import_name:
	| IMPORT dotted_as_name
	
import_from:
	| FROM dots_and_dotted_name IMPORT import_names

import_names:
	| import_as_names
	| TIMES 
	| LPAREN import_as_names RPAREN
	
dots_and_dotted_name:
	| dots_opt dotted_name 
	| dots

dots_opt:
	| {}
	| dots

dots:
	| POINT
	| POINT POINT POINT
	
import_as_name:
	| NAME AS NAME

dotted_as_name :
	| dotted_name AS NAME

import_as_names :
	| import_as_name import_as_name_opt COMMA

import_as_name_opt:
	| {}
	| import_as_name_opt {}
	| COMMA import_as_name [}

dotted_as_names:
	| dotted_as_name dotted_as_name_opt
	
dotted_as_name_opt:
	| {}
	| dotted_as_name_opt {}
	| COMMA dotted_as_name {}
	
dotted_name:
	| NAME dotted_name_opt
	
dotted_name_opt:
	| {}
	| dotted_name_opt {}
	| POINT NAME

global_stmt:
	| GLOBAL NAME comma_name_opt

comma_name_opt:
	| {}
	| comma_name_opt {}
	| COMMA NAME
	
nonlocal_stmt:
	| NONLOCAL NAME comma_name_opt {}
	
assert_stmt:
	| ASSERT test comma_test_opt

comma_test_opt:
	| {}
	| comma_test_opt {}
	| COMMA test {}
	

	
	
yield_expr:
	| YIELD yield_arg

yield_arg:
	| FROM test
	| testlist
    


