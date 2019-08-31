type token =
  | NAME of string 
  | INT of int 
  | LONGINT of int 
  | FLOAT of float
  | IMAG of string
  | STR of string
  | INDENT
  | DEDENT
  | NEWLINE
  | AND
  | AS
  | ASSERT
  | BREAK
  | CLASS
  | CONTINUE
  | DEF
  | DEL
  | ELIF
  | ELSE
  | EXCEPT
  | EXEC
  | FINALLY
  | FOR
  | FROM
  | GLOBAL
  | IF
  | IMPORT
  | IN
  | IS
  | LAMBDA
  | NOT
  | OR
  | PASS
  | PRINT
  | RAISE
  | RETURN
  | TRY
  | WHILE
  | WITH
  | YIELD
  | ADD
  | SUB
  | MULT
  | DIV
  | MOD
  | POW
  | FDIV
  | BITOR
  | BITAND
  | BITXOR
  | BITNOT
  | LSHIFT
  | RSHIFT
  | EQ
  | ADDEQ
  | SUBEQ
  | MULTEQ
  | DIVEQ
  | MODEQ
  | POWEQ
  | FDIVEQ
  | ANDEQ
  | OREQ
  | XOREQ
  | LSHEQ
  | RSHEQ
  | EQUAL
  | NOTEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | COLON
  | SEMICOL
  | DOT
  | COMMA
  | BACKQUOTE
  | AT
  | ENDMARKER

