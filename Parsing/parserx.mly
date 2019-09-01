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
%token PLUS MINUS TIMES INDENT DEDENT
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
	| fi = file_input { fi }


decorator:
	| AROBAS dotted_name arglist_paren_opt NEWLINE {}
	
arglist_paren_opt:
	| {}
	| LPAREN arglist_opt RPAREN {}
	
arglist_opt:
	| {}
	| arglist {}

decorators_opt:
	| {}
	| decorator decorators_opt {}
	
decorators:
	| decorator decorators_opt {}

decorated:
	| decorators class_func_def {}

class_func_def:
	| classdef {}
	| funcdef {}
	| async_funcdef {}
	
async_funcdef:
	| ASYNC funcdef {}
	
funcdef:
	| DEF IDENT parameters arrow_test_opt COLON suite {FunctionDef()}
	
arrow_test_opt:
	| {}
	| FUNCMETADATA test {}
	
parameters:
	| LPAREN typedargslist_opt RPAREN {}
	
typedargslist_opt:
	| {}
	| typedargslist {}

typedargslist:
	| tfpdef_init_opt tfpdef_test_opt (*comma_times_power_tfpdef_test_opt*) {}
	| POWER tfpdef comma_opt {} (*works*)
	| TIMES tfpdef_opt tfpdef_test_opt comma_tfpdef_opt {} (*works*)
	
tfpdef_init_opt:
	| tfpdef equal_test_opt {}

comma_times_power_tfpdef_test_opt:
	| {}
	| COMMA times_power_tfpdef_test_opt {}
	
times_power_tfpdef_test_opt:
	| {}
	| TIMES tfpdef tfpdef_test_opt comma_tfpdef_opt {}
	| POWER tfpdef comma_opt {}
	
comma_tfpdef_opt:
	| {}
	| COMMA power_tfpdef_opt {}
	
power_tfpdef_opt:
	| {}
	| POWER tfpdef comma_opt {}
	
tfpdef_test_opt:
	| {}
	| tfpdef_test tfpdef_test_opt {}
	
tfpdef_test:
	| COMMA tfpdef_init_opt {}
	
tfpdef_opt:
	| {}
	| tfpdef {}

tfpdef:
	| IDENT colon_test_opt {}
	
colon_test_opt:
	| {}
	| COLON test {}

varargslist:
	| vfpdef equal_test_opt vfpdef_test_opt comma_times_power_vfpdef_test_opt {}
	| POWER vfpdef comma_opt {}
	| TIMES vfpdef_opt vfpdef_test_opt comma_vfpdef_opt {}

comma_times_power_vfpdef_test_opt:
	| {}
	| COMMA times_power_vfpdef_test_opt {}
	
times_power_vfpdef_test_opt:
	| {}
	| TIMES vfpdef_opt vfpdef_test_opt comma_vfpdef_opt {}
	| POWER vfpdef comma_opt {}
	
vfpdef_test_opt:
	| {}
	| vfpdef_test vfpdef_test_opt {}
	
vfpdef_test:
	| COMMA vfpdef equal_test_opt {}
	
equal_test_opt:
	| {}
	| EQUAL test {}
	
comma_vfpdef_opt:
	| {}
	| COMMA power_vfpdef_opt {}
	
power_vfpdef_opt:
	| {}
	| POWER vfpdef comma_opt {}

vfpdef_opt:
	| {}
	| vfpdef {}
vfpdef:
	| IDENT {}

file_input:
	| new_line_stmt* EOF {}

new_line_stmt:
	| NEWLINE {}
	| stmt {}
	
stmt:
	| simple_stmt {}
	| compound_stmt {}
	
simple_stmt:
	| small_stmt small_stmt_opt* SEMICOLON? NEWLINE {}
	
small_stmt_opt:
	| SEMICOLON small_stmt {}
	
small_stmt:
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
	
continue_stmt:
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
	
import_stmt:
	| import_name {}
	| import_from {}
	
import_name:
	| IMPORT dotted_as_names {}
	
import_from:
	| FROM import_from_block IMPORT import_name_block {}
	
import_name_block:
	| TIMES {}
	| LPAREN import_as_names RPAREN {}
	| import_as_names {}
	
import_as_name:
	| IDENT as_name_opt {}
	
as_name_opt:
	| {}
	| AS IDENT {}
	
dotted_as_name:
	| dotted_name as_name_opt {}
	
import_as_names:
	| import_as_name comma_import_as_name_opt comma_opt {}
	
comma_import_as_name_opt:
	| {}
	| comma_import_as_name comma_import_as_name_opt {}
	
comma_import_as_name:
	| COMMA import_as_name {}
	
dotted_as_names:
	| dotted_as_name comma_dotted_as_name_opt {}
	
comma_dotted_as_name_opt:
	| {}
	| comma_dotted_as_name comma_dotted_as_name_opt {}
	
comma_dotted_as_name:
	| COMMA dotted_as_name {}

	
import_from_block:
	| only_dots_import {}
	| dots_name_import {}
	
only_dots_import:
	| dots dots_opt {}
	
dots_name_import:
	| dots_opt dotted_name {}
	
dotted_name:
	| IDENT dot_name_opt {}
	
dot_name_opt:
	| {}
	| POINT IDENT {}
	
global_stmt:
	| GLOBAL IDENT comma_name_opt {}
	
nonlocal_stmt:
	| NONLOCAL IDENT comma_name_opt {}
	
assert_stmt:
	| ASSERT test comma_test_opt {}

comma_test_opt:
	| {}
	| COMMA test {}
	
dots_opt:
	| {}
	| dots {}

dots:
	| POINT {}
	| POINT POINT POINT {}

comma_name_opt:
	| {}
	| comma_name comma_name_opt {}

comma_name:
	| COMMA IDENT {}
	
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
	| ASYNC with_for_funcdef_stmt {}

with_for_funcdef_stmt:
	| funcdef {}
	| with_stmt {}
	| for_stmt {}

if_stmt:
	| IF test COLON suite elif_stmt_opt else_colon_suite_opt {}

elif_stmt_opt:
	| {}
	| elif_stmt elif_stmt_opt {}
	
elif_stmt:
	| ELIF test COLON suite {}
	
else_colon_suite_opt:
	| {}
	| else_colon_suite {}
	
else_colon_suite:
	| ELSE COLON suite {}

while_stmt:
	| WHILE test COLON suite else_colon_suite_opt {}
	

for_stmt:
	| FOR exprlist IN testlist COLON suite else_colon_suite_opt {}

try_stmt:
	| TRY COLON suite try_except_clause_stmt {}
	
try_except_clause_stmt:
	| finally_colon_suite {}
	| except_clause_suite except_clause_suite_opt else_colon_suite_opt finally_colon_suite_opt {}

finally_colon_suite:
	| FINALLY COLON suite {}
	
except_clause_suite_opt:
	| {}
	| except_clause_suite except_clause_suite_opt {}
	
except_clause_suite:
	| except_clause COLON suite {}
	
finally_colon_suite_opt:
	| {}
	| finally_colon_suite {}
	
with_stmt:
	| WITH with_item comma_with_item_opt COLON suite {}
	
comma_with_item_opt:
	| {}
	| comma_with_item comma_with_item_opt {}
	
comma_with_item:
	| COMMA with_item {}
	
with_item:
	| test as_expr_opt {}
	
as_expr_opt:
	| {}
	| AS expr {}
	
except_clause:
	| EXCEPT test_as_test_opt {}
	
test_as_test_opt:
	| {}
	| test as_name_opt {}

suite:
	| simple_stmt {}
	| NEWLINE INDENT stmt+ DEDENT {}

expr_stmt:
	| testlist_star_expr super_expr_stmt {}

super_expr_stmt:
	| annassign {}
	| augassign yield_or_testlist {}
	| assign_stmt {}

assign_stmt_expr:
	| {}
	| assign_stmt assign_stmt_expr {}

assign_stmt:
	| EQUAL yield_expr {}
	| EQUAL testlist_star_expr {}

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
	| star_expr {}

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
	| operator_factor {}
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
	| LPAREN yield_or_testlist_expr? RPAREN {} 
	| LBRACK testlist_comp? RBRACK {}
	(*| LBRACE dictorsetmaker RBRACE {}*)
	| IDENT {}
	| INTEGERLIT {}
	| STRINGLIT+ {}
	| POINT POINT POINT {}
	| NONE {}
	| BOOLEANLIT {}

yield_or_testlist_expr:
	| yield_expr {}
	| testlist_comp {}

yield_expr:
	| YIELD yield_arg? {}

yield_arg:
	| FROM test {}
	| testlist {}
    


trailer:
	| LPAREN arglist? RPAREN {}
	| LBRACK subscriptlist? RBRACK {}
	| POINT IDENT {}

arglist:
	| argument argument_opts comma_opt {}

comma_opt:
	| {}
	| COMMA {}

argument_opts:
	| {}
	| argument_opt argument_opts {}

argument_opt:
	| {}
	| COMMA argument {}

argument:
	| test comp_for? {}
	| test EQUAL test {}
	| POWER test {}
	| TIMES test {}

test:
	| or_test or_test_conditional {}
	| lambdef {}
	
or_test_conditional:
	| {}
	| IF or_test ELSE test {}


test_nocond: 
	| or_test {}
	| lambdef_nocond {}

lambdef:
	| LAMBDA varargslist COLON test {}

lambdef_nocond:
	| LAMBDA varargslist COLON test_nocond {}

or_test:
	| and_test or_and_test_opt* {}

or_and_test_opt:
	| OR and_test {}

and_test:
	| not_test and_not_test_opt* {}

and_not_test_opt:
	| AND not_test {}
	
not_test:
	| NOT not_test {}
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
	
testlist_comp:
	| test_or_star_expr comp_for_or_test_start_expr_opt {}
	
comp_for_or_test_start_expr_opt:
	| {}
	| comp_for_or_test_star_expr comp_for_or_test_start_expr_opt {}
	
comp_for_or_test_star_expr:
	| comp_for {}
	| test_or_star_expr_opts comma_opt {}
	
	
subscriptlist:
	| subscript comma_subscript_opt comma_opt {}
	
comma_subscript_opt:
	| {}
	| comma_subscript comma_subscript_opt {}
	
comma_subscript:
	| COMMA subscript {}
	
subscript:
	| test {}
	| test_opt COLON test_opt sliceop_opt {}
	
test_opt:
	| {}
	| test {}
	
sliceop_opt:
	| {}
	| sliceop {}
	
sliceop:
	| COLON test_opt {}
	
exprlist:
	| expr_or_start_expr comma_expr_or_star_expr_opt comma_opt {}
	

expr_or_start_expr:
	| expr {}
	| star_expr {}
	
comma_expr_or_star_expr_opt:
	| {}
	| comma_expr_or_star_expr comma_expr_or_star_expr_opt {}
	
comma_expr_or_star_expr:
	| COMMA expr_or_start_expr {}
	
testlist:
	| test comma_test_testlist comma_opt {}
	
comma_test_testlist:
	| {}
	| comma_test comma_test_testlist {}
	
comma_test:
	| COMMA test {}
	
classdef:
	| CLASS IDENT arglist_paren_opt COLON suite {}
	
comp_iter:
	| comp_for {}
	| comp_if {}
	
sync_comp_for:
	| FOR exprlist IN or_test comp_iter_opt {}
	
comp_iter_opt:
	| {}
	| comp_iter {}
	
comp_for:
	| async_opt sync_comp_for {}
	
async_opt:
	| {}
	| ASYNC {}
	
comp_if:
	| IF test_nocond comp_iter_opt {}