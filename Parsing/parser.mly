%{
	open Ast
%}

/* literals */
%token <string>   NAME
%token <int>      INTEGERLIT
%token <string>   STRINGLIT
%token <bool>	  BOOLEANLIT
%token <int>      LONGINT

/* layout */
%token INDENT
%token DEDENT
%token NEWLINE

/* keywords */
%token  CLASS
%token  CONTINUE
%token  DEF
%token  DEL
%token  ELSE
%token  FOR
%token  FROM
%token  IF
%token  IN
%token  PRINT
%token  RETURN
%token  AND
%token  OR




/* symbols */
%token  ADD            /* + */
%token  MINUS            /* - */
%token  TIMES           /* * */
%token  DIV            /* / */
%token  MOD            /* % */
%token  POWER            /* ** */


%token  EQ             /* = */
%token  ADDEQ          /* += */
%token  SUBEQ          /* -= */
%token  MULTEQ         /* *= */
%token  DIVEQ          /* /= */
%token  MODEQ          /* %= */
%token  POWEQ          /* **= */


%token  EQUAL          /* == */
%token  NOTEQ          /* !=, <> */
%token  LT             /* < */
%token  GT             /* > */
%token  LEQ            /* <= */
%token  GEQ            /* >= */

%token  LPAREN         /* ( */
%token  RPAREN         /* ) */
%token  LBRACK         /* [ */
%token  RBRACK         /* ] */
%token  LBRACE         /* { */
%token  RBRACE         /* } */
%token  COLON          /* : */
%token  DOT            /* . */
%token  COMMA          /* , */

/* eof */
%token ENDMARKER

%token NONE



%start file_input
%type <Ast.stmt list> file_input 
%%

file_input:
	| statement_list ENDMARKER { $1 }

statement_list:
	| { [] }
	| NEWLINE statement_list { $2 }
	| stmt statement_list { $1 @ $2 }

stmt_list:
	| { [] }
	| stmt stmt_list { $1 @ $2 }

stmt:
	| simple_stmt { $1 }
	| compound_stmt { [$1] }

(* simple statements *)

simple_stmt:
	| small_stmt NEWLINE { [$1] }

small_stmt:
	| expr_stmt { $1 }
	| print_stmt { $1 }
	| return_stmt { $1 }


return_stmt:
	| RETURN expr { Return($2) }

expr_stmt:
	| name EQ expr { Assign( $1, $3) }
	| call { Assign("None", $1) }

expr :
	| test { $1 }
	| arith_expr { $1 }
	| conditional_expr { $1 }

arith_expr:
	| test bin_operators expr { BinOp($1,$2,$3) }


conditional_expr:
	| test comp_operators expr { Compare($1, $2, $3) }
	| test bool_operators expr { BoolOp($1, $2, $3) }

call :
	| name LPAREN separated_list(COMMA, test) RPAREN { Call($1, $1, $3) }
	| name DOT name LPAREN separated_list(COMMA, test) RPAREN { Call($1, $3, $5) }

print_stmt:
	| PRINT LPAREN expr RPAREN { Print($3) }



test :
	| value { $1 }
	| name { Name($1,Load) }
	| name DOT name {Name($1^"."^$3,Load)}
	| call { $1 }
	| LBRACK separated_list(COMMA, test) RBRACK { List($2, Load)}




(*compound statement*)

compound_stmt:
	| funcdef { $1 }
	| classdef { $1 }
	| if_stmt { $1 }
	| for_stmt { $1 }

for_stmt:
	| FOR name IN test COLON suite { For($2, $4, $6)}

if_stmt:
	| IF expr COLON suite { If($2, $4, []) }
	| IF expr COLON suite ELSE COLON suite { If($2, $4, $7) } 

classdef:
	| CLASS name COLON suite { ClassDef($2, $4) }

funcdef:
	| DEF name parameters COLON suite { FunctionDef($2, $3, $5) }



suite:
	| simple_stmt { $1 }
	| NEWLINE INDENT stmt_list DEDENT { $3 }

parameters:
	| LPAREN arglist RPAREN { $2 }


arglist:
	| separated_list(COMMA, name) { $1 }

name:
	| NAME { $1 }

value:
	| INTEGERLIT { Num(Int($1))}
	| STRINGLIT { Str($1) }
	| BOOLEANLIT { Bool($1) }
	| LONGINT { Num(Int($1)) }

bin_operators :
	| ADD { Add }
	| MINUS { Sub }
	| TIMES { Mult }
	| MOD { Mod }
	| DIV { Div }
	| POWER { Pow }

comp_operators:
	| EQUAL { Eq }
	| NOTEQ { NotEq }
	| LEQ   { LtE }
	| GEQ   { GtE }
	| LT    { Lt }
	| GT    { Gt }

bool_operators:
	| AND { And }
	| OR { Or }

