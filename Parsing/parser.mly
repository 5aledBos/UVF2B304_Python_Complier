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
	| file_input {}


(*decorator:
	| AROBAS dotted_name LPAREN arglist RPAREN NEWLINE {}

decorator_opt:
	| {}
	| decorator {}
	
decorators:
	| decorator decorator_opt {}*)
file_input:
	| new_line_stmt* EOF {}

new_line_stmt:
	| NEWLINE {}
	| stmt {}
	
stmt:
	| simple_stmt {}
	(* | compound_stmt {} *)
	
simple_stmt:
	| small_stmt small_stmt_opt* SEMICOLON? NEWLINE {}
	
small_stmt_opt:
	| SEMICOLON small_stmt {}
	
small_stmt :
	| expr_stmt {}
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
	
continue_stmt
	| CONTINUE {}
	
return_stmt:
	| RETURN testlist_opt {}

testlist_opt:
	| {}
	| testlist {}
	
yield_stmt:
	| yield_expr {}
	
raise_stmt:
	| RAISE test_from_test_opt {}

test_from_test_opt:
	| {}
	| test from_test_opt {}

from_test_opt:
	| {}
	| FROM test {}


expr_stmt:
	| testlist_star_expr super_expr_stmt {}

super_expr_stmt:
	| annassign {}
	| augassign yield_or_testlist {}
	| assign_stmt {}

augassign:
	| PLUSEQUAL {}
	| MINUSEQUAL {}
	| TIMESEQUAL {}
	| DIVEQUAL {}
	| ANDEQUAL {}
	| OREQUAL {}
	| XOREQUAL {}
	| RSHIFTEQUAL {}
	| LSHIFTEQUAL {}
	| POWEREQUAL {}
	| FLOORDIVEQUAL {}

yield_or_testlist:
	| testlist {}
	| yield_expr {}

assign_statement:
	| EQUAL yield_or_testlist_start* {}

yield_or_testlist_start:
	| yield_expr {}
	| testlist_star_expr {}

annassign:
	| COLON test annassign_opt? {}

annassign_opt:
	| EQUAL test {}

testlist_star_expr:
	| test_or_star_expr test_or_star_expr_opts comma_opt {}



test_or_star_expr:
	| test {}
	(*| star_expr {}*)

test_or_star_expr_opts:
	| {}
	| test_or_star_expr_opt test_or_star_expr_opts {}


test_or_star_expr_opt:
	| COMMA test_or_star_expr {}


star_expr:
	| TIMES expr {}

expr:
	| xor_expr xor_expr_opt* {}

xor_expr_opt:
	| BITEOR xor_expr {}

xor_expr:
	| and_expr and_expr_opt* {}

and_expr_opt:
	| BITEXOR and_expr {}

and_expr:
	| shift_expr shift_expr_opt* {}

shift_expr_opt:
	| BITEAND shift_expr {}

shift_expr:
	| arith_expr arith_expr_opt* {}

arith_expr_opt:
	| LSHIFT arith_expr {}
	| RSHIFT arith_expr {}

arith_expr:
	| term term_opt* {}

term_opt:
	| PLUS term {}
	| MINUS term {}

term:
	| factor factor_opt* {}

factor_opt:
	| TIMES factor {}
	| AROBAS factor {}
	| DIV factor {}
	| FLOORDIV factor {}
	| MOD factor {}

factor:
	(*| operator_factor {}*)
	| power {}

operator_factor:
	| PLUS factor {}
	| MINUS factor {}
	| BITENEGATION factor {}

power:
	| atom_expr power_factor? {}

power_factor:
	| POWER factor {}

atom_expr:
	| AWAIT? atom trailer* {}

atom:
	(* | LPAREN yield_or_testlist_expr? RPAREN {} *)
	(* | LBRACK testlist_comp? RBRACK {} *)
	(* | LBRACE dictorsetmaker RBRACE {} *)
	| IDENT {}
	| INTEGERLIT {}
	| STRINGLIT+ {}
	| POINT POINT POINT {}
	| NONE {}
	| BOOLEANLIT {}

yield_or_testlist_expr:
	| yield_expr {}
(*	| testlist_comp {}*)

yield_expr:
	| YIELD yield_arg? {}

yield_arg:
	| FROM test {}
	(*| testlist {}*)
    


trailer:
	| LPAREN arglist? RPAREN {}
	(*| LBRACK subscriptlist? RBRACK {}*)
	| POINT IDENT {}

arglist:
	| argument argument_opts comma_opt {}
	(* | argument argument_opt* COMMA? {} *)

comma_opt:
	| {}
	| COMMA {}

argument_opts:
	| {}
	| argument_opt argument_opts {}

argument_opt:
	| COMMA argument {}

argument:
	(*| test comp_for? {}*) 
	| test EQUAL test {}
	| POWER test {}
	| TIMES test {}

test:
	| or_test or_test_conditional {}
	(*| lambdef {}*)
	
or_test_conditional:
	| {}
	| IF or_test ELSE test {}

(*
test_nocond: 
	| or_test
	| lambdef_nocond

lambdef:
	| LAMBDA varargslist COLON test

lambdef_nocond:
	| LAMBDA varargslist COLON test_nocond {}
*)
or_test:
	| and_test or_and_test_opt* {}

or_and_test_opt:
	| OR and_test {}

and_test:
	| not_test and_not_test_opt* {}

and_not_test_opt:
	| AND not_test {}
	
not_test:
	(*| NOT not_test {}*)
	| comparison {}
	
comparison:
	| expr comp_op_expr_opt* {}

comp_op_expr_opt:
	| comp_op expr {}

comp_op:
	| INF {}
	| SUP {}
	| ISEQUAL {}
	| SUPEQUAL {}
	| INFEQUAL {}
	| ISNOTEQUAL {}
	| IN {}
	| NOT IN {}
	| IS {}
	| IS NOT {}
	



(* 

	
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
	
compound_stmt:
	| if_stmt {}
	| while_stmt {}
	| for_stmt {}
	| try_stmt {}
	| with_stmt {}
	| funcdef {}
	| classdef {}
	| decorated {}
	| async_stmt {}

async_stmt:
	| ASYNC with_for_funcdef_stmt

with_for_funcdef_stmt:
	| funcdef
	| with_stmt
	| for_stmt

if_stmt:
	| IF test COLON suite elif_stmt_opt ELSE COLON suite

elif_stmt_opt:
	| {}
	| ELIF test COLON suite

while_stmt:
	| WHILE test COLON suite ELSE COLON suite

for_stmt:
	| FOR exprlist IN testlist COLON suite ELSE COLON suite
	
white_stmt:
	| WITH with_item comma_with_item_opt COLON suite
	
comma_with_item_opt:
	| {}
	| COMMA with_item
	
with_item:
	| test AS expr_stmt

suite:
	| simple_stmt
	| NEWLINE IDENT stmt DEDENT
	
*)
