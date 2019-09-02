%{
	open Ast
%}

/* literals */
%token <string>   NAME
%token <int>      INTEGERLIT
%token <int>      LONGINT
%token <float>    FLOAT
%token <string>   IMAG
%token <string>   STRINGLIT
%token <bool>	  BOOLEANLIT

/* layout */
%token INDENT
%token DEDENT
%token NEWLINE

/* keywords */
%token  AND
%token  AS
%token  ASSERT
%token  ASYNC
%token  BREAK
%token  CLASS
%token  CONTINUE
%token  DEF
%token  DEL
%token  ELIF
%token  ELSE
%token  EXCEPT
%token  EXEC
%token  FINALLY
%token  FOR
%token  FROM
%token  GLOBAL
%token  IF
%token  IMPORT
%token  IN
%token  IS
%token  LAMBDA
%token  NOT
%token  OR
%token  PASS
%token  PRINT
%token  RAISE
%token  RETURN
%token  TRY
%token  WHILE
%token  WITH
%token  YIELD

%token FUNCMETADATA

/* symbols */
%token  ADD            /* + */
%token  MINUS            /* - */
%token  TIMES           /* * */
%token  DIV            /* / */
%token  MOD            /* % */
%token  POWER            /* ** */
%token  FDIV           /* // */
%token  BITOR          /* | */
%token  BITAND         /* & */
%token  BITXOR         /* ^ */
%token  BITNOT         /* ~ */
%token  LSHIFT         /* << */
%token  RSHIFT         /* >> */

%token  EQ             /* = */
%token  ADDEQ          /* += */
%token  SUBEQ          /* -= */
%token  MULTEQ         /* *= */
%token  DIVEQ          /* /= */
%token  MODEQ          /* %= */
%token  POWEQ          /* **= */
%token  FDIVEQ         /* //= */
%token  ANDEQ          /* &= */
%token  OREQ           /* |= */
%token  XOREQ          /* ^= */
%token  LSHEQ          /* <<= */
%token  RSHEQ          /* >>= */

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
%token  SEMICOLON      /* ; */
%token  DOT            /* . */
%token  COMMA          /* , */
%token  BACKQUOTE      /* ` */
%token  AT             /* @ */

/* eof */
%token ENDMARKER

%token NONE
%token AWAIT
%token NONLOCAL


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
	| stmt stmt_list {$1 @ $2 }

stmt:
	| simple_stmt { $1 }
	| compound_stmt { [$1] }

(* simple statements *)

simple_stmt:
	| small_stmt NEWLINE { [$1] }

small_stmt:
	| expr_stmt { $1 }
	| print_stmt { $1 }




expr_stmt:
	| name EQ value { Assign( $1, $3) }
	| name EQ call { Assign($1, $3) }
	| call { Assign("None", $1) }

call :
	| name parameters { Call($1, $1, $2) }
	| name DOT name parameters { Call($1, $3, $4) }

print_stmt:
	| PRINT LPAREN test RPAREN { Print($3) }

value:
	| INTEGERLIT {Num(Int($1))}
	| STRINGLIT { Str($1) }

test :
	| name { Name($1,Load) }
	| value { $1 }
	| name DOT name {Name($1^"."^$3,Load)}
	| call { $1 }


(*compound statement*)

compound_stmt:
	| funcdef { $1 }
	| classdef { $1 }

classdef:
	| CLASS name COLON suite { ClassDef($2, $4) }

funcdef:
	| DEF name parameters COLON suite { FunctionDef($2, $3, $5) }

name:
	| NAME { $1 }


suite:
	| simple_stmt { $1 }
	| NEWLINE INDENT stmt_list DEDENT { $3 }

parameters:
	| LPAREN arglist RPAREN { $2 }


arglist:
	| {[]}
	| fpdef { [$1] }
(*	| fpdef COMMA arglist {
			match $3 with
			  | args -> $1::args}*)

fpdef:
	| NAME { Name($1, Param) }
	(*| LPAREN fplist RPAREN { $1 }*)

fplist:
	| fpdef { [$1] }
	| fpdef COMMA { [$1] }
	| fpdef COMMA fplist { $1::$3 }


