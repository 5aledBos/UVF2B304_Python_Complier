{
open Lexing
open Parser
open ErrorHandler

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
	"else ",		ELSE;
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
	"nonlocal", 	NONLOCAL;
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
let white     							= [' ' '\t']+
let lowercase							= ['a'-'z']
let uppercase							= ['A'-'Z']
let newline   							= '\r' | '\n' | "\r\n"
let letter							    = (lowercase | uppercase)
let shortStringchar						= [^'\r' '\n' ''' '"' '\\']*
let shortstring 						= ''' shortStringchar ''' | '"'  shortStringchar '"'
let stringEscapeSeq						= [^'\\']*
let nonzerodigit						= ['1'-'9']
let digit								= '0' | nonzerodigit
let decinteger							= nonzerodigit (['_'] digit)* | '0' (['_'] '0')*
let digitPart 							= digit (['_'] digit)*
let exponent 							= ('e' | 'E') ('+' | '-')? digit
let fraction 							= '.' digitPart
let pointFloat							=  digitPart fraction | digitPart '.'
let exponentFloat 						= (digitPart | pointFloat) exponent
let floatNumber 						= pointFloat | exponentFloat
let imagNumber 							= (floatNumber | digitPart) ('j' | 'J')

let ident     						= (letter | '_') ( letter | digit | '_')*



rule read = parse
| white											{ read lexbuf }
| newline										{ Lexing.new_line lexbuf; read lexbuf }
| decinteger as i				{ INTEGERLIT (int_of_string i) }
| floatNumber as f	{ FLOATLIT (float_of_string f) }
| shortstring as s				{ STRINGLIT s}
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
{

let print_token = function
| EOF                -> print_string "eof"
| IDENT id           -> print_string "(ident : "; print_string id; print_string ")"
| FLOATLIT f         -> print_string "(float : "; print_float f; print_string ")"
| INTEGERLIT i			 -> print_string "(integer :"; print_int i; print_string ")"
| BOOLEANLIT b       -> ( match b with
| true							 -> print_string "(boolean : true)"
| false							 -> print_string "(boolean : false)")
| STRINGLIT s				 -> print_string("(string : "^s^")")
| COMMENT c					 -> ()
| NULL               -> print_string "null"
| ABSTRACT           -> print_string "abstract"
| ASSERT             -> print_string "assert"
| BOOLEAN            -> print_string "boolean"
| BREAK              -> print_string "break"
| BYTE               -> print_string "byte"
| CASE               -> print_string "case"
| CATCH              -> print_string "catch"
| CHAR               -> print_string "char"
| CLASS              -> print_string "class"
| CONTINUE           -> print_string "continue"
| DEFAULT            -> print_string "default"
| DO                 -> print_string "do"
| DOUBLE             -> print_string "double"
| ELSE               -> print_string "else"
| ENUM               -> print_string "enum"
| EXTENDS            -> print_string "extends"
| FINAL              -> print_string "final"
| FINALLY            -> print_string "finally"
| FLOAT              -> print_string "float"
| FOR                -> print_string "for"
| IF                 -> print_string "if"
| IMPLEMENTS         -> print_string "implements"
| IMPORT             -> print_string "import"
| INSTANCEOF         -> print_string "instanceof"
| INT                -> print_string "int"
| INTERFACE          -> print_string "interface"
| LONG               -> print_string "long"
| NATIVE             -> print_string "native"
| NEW                -> print_string "new"
| PACKAGE            -> print_string "package"
| PRIVATE            -> print_string "private"
| PROTECTED          -> print_string "protected"
| PUBLIC             -> print_string "public"
| RETURN             -> print_string "return"
| SHORT              -> print_string "short"
| STATIC             -> print_string "static"
| STRICTFP           -> print_string "strictfp"
| SUPER              -> print_string "super"
| SWITCH             -> print_string "switch"
| SYNCHRONIZED       -> print_string "synchronized"
| THIS               -> print_string "this"
| THROW              -> print_string "throw"
| THROWS             -> print_string "throws"
| TRANSIENT          -> print_string "transient"
| TRY                -> print_string "try"
| VOID               -> print_string "void"
| VOLATILE           -> print_string "volatile"
| WHILE              -> print_string "while"
| PLUS               -> print_string "plus"
| MINUS              -> print_string "minus"
| TIMES              -> print_string "times"
| DIV                -> print_string "div"
| AND                -> print_string "and"
| OR                 -> print_string "or"
| XOR                -> print_string "xor"
| MOD                -> print_string "mod"
| EQUAL              -> print_string "equal"
| INF                -> print_string "inf"
| SUP                -> print_string "sup"
| CONDOR             -> print_string "condor"
| CONDAND            -> print_string "condand"
| INCR               -> print_string "incr"
| DECR               -> print_string "decr"
| COND               -> print_string "cond"
| EXCL               -> print_string "excl"
| TILDE              -> print_string "tilde"
| AROBAS              -> print_string "arobas"
| ISEQUAL            -> print_string "isequal"
| ISNOTEQUAL         -> print_string "isnotequal"
| PLUSEQUAL          -> print_string "plusequal"
| MINUSEQUAL         -> print_string "minusequal"
| TIMESEQUAL         -> print_string "timesequal"
| DIVEQUAL           -> print_string "divequal"
| ANDEQUAL           -> print_string "andequal"
| OREQUAL            -> print_string "orequal"
| XOREQUAL           -> print_string "xorequal"
| MODEQUAL           -> print_string "modequal"
| INFEQUAL           -> print_string "infequal"
| SUPEQUAL           -> print_string "supequal"
| LSHIFT             -> print_string "lshift"
| RSHIFT             -> print_string "rshift"
| LSHIFTEQUAL        -> print_string "lshiftequal"
| RSHIFTEQUAL        -> print_string "rshiftequal"
| USHIFT             -> print_string "ushift"
| USHIFTEQUAL        -> print_string "ushiftequal"
| POINT              -> print_string "point"
| SEMICOLON          -> print_string "semicolon"
| COMMA              -> print_string "comma"
| COLON              -> print_string "colon"
| LBRACE             -> print_string "lbrace"
| RBRACE             -> print_string "rbrace"
| LPAREN             -> print_string "lparen"
| RPAREN             -> print_string "rparen"
| LBRACK             -> print_string "lbrack"
| RBRACK             -> print_string "rbrack"

}
