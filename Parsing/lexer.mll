{
open Lexing
open Parser
open ErrorHandler
open Lexer_state


let curr_pos lexbuf = lexbuf.lex_start_p

  let count_lines s =
    let n = ref 0 in
      String.iter
        (fun c -> if c = '\n' then incr n)
        s;
!n

let keyword_table = Hashtbl.create 15
let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
[
	(* macro *)
	"False",		BOOLEANLIT false;
	"None", 		NONE;
	"True",			BOOLEANLIT true;
	"and", 			AND;
	"as", 			AS;
	"assert",		ASSERT;
	"async", 		ASYNC;
	"break",		BREAK;
	"class",		CLASS;
	"continue",		CONTINUE;
 	"def", 			DEF;
	"del", 			DEL;
	"elif", 		ELIF;
	"else",			ELSE;
	"except", 		EXCEPT;
	"finally", 		FINALLY;
	"for",			FOR;
	"from", 		FROM;
	"global", 		GLOBAL;
	"if",			IF;
	"import",		IMPORT;
	"in", 			IN;
	"is", 			IS;
	"lambda", 		LAMBDA;
	"nonlocal", 		NONLOCAL;
	"not", 			NOT;
	"or", 			OR;
	"pass", 		PASS;
	"raise", 		RAISE;
	"return",		RETURN;
	"try", 			TRY;
	"while",		WHILE;
	"with", 		WITH;
	"yield", 		YIELD;
	"super",		SUPER;
	"await", 		AWAIT;
]

}
let e = ""
let whitespace     							= [' ']+
let tab 								= ['	' '\t']
let indent_catcher 						= "\n"tab*
let lowercase							= ['a'-'z']
let uppercase							= ['A'-'Z']
let newline   							= ('\r' | '\n' | "\r\n")+
let letter							    = (lowercase | uppercase)
let shortStringchar						= [^'\r' '\n' ''' '"' '\\']+
let shortstring 						= ''' shortStringchar ''' | '"'  shortStringchar '"'
let longStringText 						= (shortStringchar | newline | tab)+
let longstring 							= ''' ''' ''' longStringText ''' ''' ''' | '"' '"' '"' longStringText '"' '"' '"'  
let stringEscapeSeq						= [^'\\']*
let nonzerodigit						= ['1'-'9']
let digit								= '0' | nonzerodigit
let decinteger							= nonzerodigit (['_'] | digit)* | '0' (['_'] '0')*
let digitPart 							= digit (['_'] digit)*
let exponent 							= ('e' | 'E') ('+' | '-')? digit
let fraction 							= '.' digitPart
let pointFloat							=  digitPart fraction | digitPart '.'
let exponentFloat 						= (digitPart | pointFloat) exponent
let floatNumber 						= pointFloat | exponentFloat
let imagNumber 							= (floatNumber | digitPart) ('j' | 'J')

let ident     							= (letter | '_') ( letter | digit | '_')*
let comment 						= '#' (ident | whitespace)+



rule read state = parse
  | e { let curr_offset = state.curr_offset in
        let last_offset = Stack.top state.offset_stack in
          if curr_offset < last_offset
          then (ignore (Stack.pop state.offset_stack); DEDENT)
          else if curr_offset > last_offset
          then (Stack.push curr_offset state.offset_stack; INDENT)
	else token state lexbuf }
	

and token state = parse
  | ((whitespace* comment? newline)* whitespace* comment?) newline
      { let lines = count_lines (lexeme lexbuf) in
        let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + lines };
        if state.nl_ignore <= 0 then begin
          state.curr_offset <- 0;
          offset state lexbuf;
          NEWLINE
        end else
          token state lexbuf }
  | '\\' newline whitespace*
      { let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + 1 };
          token state lexbuf }

  | whitespace+
{ token state lexbuf }
| comment as c								{ COMMENTLINE (c) }
| newline										{ NEWLINE }
| decinteger as i				{ INTEGERLIT (int_of_string i) }
| floatNumber as f	{ FLOATLIT (float_of_string f) }
| shortstring as s				{ STRINGLIT s}
| longstring as t				{ STRINGLONG t}
| ident as id								{  try Hashtbl.find keyword_table id with Not_found -> IDENT id }
| "+"                { PLUS }
| "-"                { MINUS }
| "*"                { TIMES }
| "**"               { POWER }
| "/"                { DIV }
| "//"               { FLOORDIV }
| "%"                { MOD }
| "@"                { AROBAS }
| "<<"               { LSHIFT }
| ">>"               { RSHIFT }
| "&"                { BITEAND }
| "|"                { BITEOR }
| "^"                { BITEXOR }
| "~"                { BITENEGATION }
| "<"                { INF }
| ">"                { SUP }
| "<="               { INFEQUAL }
| ">="               { SUPEQUAL }
| "=="               { ISEQUAL }
| "!="               { ISNOTEQUAL }
| "("                { LPAREN }
| ")"                { RPAREN }
| "{"                { LBRACE }
| "}"                { RBRACE }
| "["                { LBRACK }
| "]"                { RBRACK }
| "."                { POINT }
| ";"                { SEMICOLON }
| ","                { COMMA }
| ":"                { COLON }
| "="                { EQUAL }
| "->"               { FUNCMETADATA }
| "+="               { PLUSEQUAL }
| "-="               { MINUSEQUAL }
| "*="               { TIMESEQUAL }
| "/="               { DIVEQUAL } 
| "//="              { FLOORDIVEQUAL }
| "%="               { MODEQUAL }
| "@="               { AROBASQUAL }
| "&="               { ANDEQUAL }
| "|="               { OREQUAL }
| "^="               { XOREQUAL }
| ">>="              { RSHIFTEQUAL }
| "<<="              { LSHIFTEQUAL }
| "**="              { POWEREQUAL }
| "'"              	 { SIMPLEQUOTE }
| '"'              	 { DOUBLEQUOTE }
| "#"              	 { SHARP }
| "\""               { BACKSLASH }
| "$"              	 { DOLLAR }
| "?"              	 { QUESTIONMARK }
| eof                { EOF }
and offset state = parse
  | e { }
  | ' '  { state.curr_offset <- state.curr_offset + 1; offset state lexbuf }
  | '\t' { state.curr_offset <- state.curr_offset + 8; offset state lexbuf }

{

let print_token = function
| EOF                		-> print_string "<eof>"
| NEWLINE 					-> print_newline () 
| IDENT id           		-> print_string "(ident : "; print_string id; print_string ")"
| COMMENTLINE c 			-> print_string "(comment :"; print_string c; print_string ")"
| FLOATLIT f         		-> print_string "(float : "; print_float f; print_string ")"
| INTEGERLIT i 				-> print_string "(integer :"; print_int i; print_string ")"
| BOOLEANLIT b       		-> ( match b with
	| true 					-> print_string "(boolean : true)"
	| false 				-> print_string "(boolean : false)")
| STRINGLIT s				-> print_string("(string : "^s^")")
| STRINGLONG t				-> print_string("(text : "^t^")")
| POWER			     		-> print_string "< power >"
| AS 						-> print_string "< as >"
| ASYNC 					-> print_string "< async >"
| DEL 						-> print_string "< del >"
| ELIF 						-> print_string "< elif >"
| EXCEPT 					-> print_string "< except >"
| FROM 						-> print_string "< from >"
| GLOBAL 					-> print_string "< global >"
| IN 						-> print_string "< in >"
| IS 						-> print_string "< is >"
| LAMBDA 					-> print_string "< lambda >"
| NONLOCAL 					-> print_string "< nonlocal >"
| NOT 						-> print_string "< not >"
| PASS 						-> print_string "< pass >"
| RAISE 					-> print_string "< raise >"
| WITH 						-> print_string "< with >"
| YIELD 					-> print_string "< yield >"
| AWAIT 					-> print_string "< await >"
| FLOORDIV			     	-> print_string "< floordiv >"
| BITEAND				    -> print_string "< biteand >"
| BITEOR				    -> print_string "< biteor >"
| BITEXOR				    -> print_string "< bitexor >"
| BITENEGATION			    -> print_string "< bitenegation >"
| FUNCMETADATA			    -> print_string "< funcmetadata >"
| FLOORDIVEQUAL			    -> print_string "< floordivequal >"
| AROBASQUAL			    -> print_string "< arobasqual >"
| POWEREQUAL			    -> print_string "< powerequal >"
| SIMPLEQUOTE			    -> print_string "< simplequote >"
| DOUBLEQUOTE			    -> print_string "< doublequote >"
| SHARP			     		-> print_string "< sharp >"
| BACKSLASH			     	-> print_string "< backslash >"
| DOLLAR			     	-> print_string "< dollar >"
| QUESTIONMARK			    -> print_string "< questionmark >"
| NONE               		-> print_string "< none >"
| ASSERT             		-> print_string "< assert >"
| BOOLEAN            		-> print_string "< boolean >"
| BREAK              		-> print_string "< break >"
| CLASS              		-> print_string "< class >"
| DEF              	 		-> print_string "< def >"
| CONTINUE           		-> print_string "< continue >"
| ELSE               		-> print_string "< else >"
| FINALLY            		-> print_string "< finally >"
| FLOAT              		-> print_string "< float >"
| FOR                		-> print_string "< for >"
| IF                 		-> print_string "< if >"
| IMPORT             		-> print_string "< import >"
| RETURN             		-> print_string "< return >"
| SUPER              		-> print_string "< super >"
| TRY                		-> print_string "< try >"
| WHILE              		-> print_string "< while >"
| PLUS               		-> print_string "< plus >"
| MINUS              		-> print_string "< minus >"
| TIMES              		-> print_string "< times >"
| DIV                		-> print_string "< div >"
| AND                		-> print_string "< and >"
| OR                 		-> print_string "< or >"
| XOR                		-> print_string "< xor >"
| MOD                		-> print_string "< mod >"
| EQUAL              		-> print_string "< equal >"
| INF                		-> print_string "< inf >"
| SUP                		-> print_string "< sup >"
| CONDOR             		-> print_string "< condor >"
| CONDAND            		-> print_string "< condand >"
| INCR               		-> print_string "< incr >"
| DECR               		-> print_string "< decr >"
| COND               		-> print_string "< cond >"
| AROBAS             		-> print_string "< arobas >"
| ISEQUAL            		-> print_string "< isequal >"
| ISNOTEQUAL         		-> print_string "< isnotequal >"
| PLUSEQUAL          		-> print_string "< plusequal >"
| MINUSEQUAL         		-> print_string "< minusequal >"
| TIMESEQUAL         		-> print_string "< timesequal >"
| DIVEQUAL           		-> print_string "< divequal >"
| ANDEQUAL           		-> print_string "< andequal >"
| OREQUAL            		-> print_string "< orequal >"
| XOREQUAL           		-> print_string "< xorequal >"
| MODEQUAL           		-> print_string "< modequal >"
| INFEQUAL           		-> print_string "< infequal >"
| SUPEQUAL           		-> print_string "< supequal >"
| LSHIFT             		-> print_string "< lshift >"
| RSHIFT             		-> print_string "< rshift >"
| LSHIFTEQUAL        		-> print_string "< lshiftequal >"
| RSHIFTEQUAL        		-> print_string "< rshiftequal >"
| USHIFT             		-> print_string "< ushift >"
| USHIFTEQUAL        		-> print_string "< ushiftequal >"
| POINT              		-> print_string "< point >"
| SEMICOLON          		-> print_string "< semicolon >"
| COMMA              		-> print_string "< comma >"
| COLON              		-> print_string "< colon >"
| LBRACE             		-> print_string "< lbrace >"
| RBRACE             		-> print_string "< rbrace >"
| LPAREN             		-> print_string "< lparen >"
| RPAREN             		-> print_string "< rparen >"
| LBRACK             		-> print_string "< lbrack >"
| RBRACK             		-> print_string "< rbrack >"
| TAB  			 	-> print_string "tab"
| INDENT 			-> print_string "indent"
| DEDENT			-> print_string "dedent"
| _				-> print_string "something else"

}
