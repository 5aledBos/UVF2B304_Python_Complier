{
  open Lexer_state
  open Lexing
  open Parser


  let unescaped s =
    let buf = Buffer.create (String.length s) in
    let escape = ref false in
    let unescapechar c =
      if !escape then begin
        match c with
        | '\r' -> ()
        | '\n' -> escape := false
        | _ -> begin
            escape := false;
            (* TODO http://docs.python.org/reference/lexical_analysis.html#string-literals *)
            Buffer.add_char
              buf
              (match c with
               | '\\' -> '\\'
               | '\'' -> '\''
               | '"' -> '"'
               | 'a' -> Char.chr 7
               | 'b' -> '\b'
               | 'f' -> Char.chr 12
               | 'n' -> '\n'
               | 'r' -> '\r'
               | 't' -> '\t'
               | 'v' -> Char.chr 11
               | _ -> (Buffer.add_char buf '\\'; c))
          end
      end else if c = '\\' then
        escape := true
      else
        Buffer.add_char buf c
    in
      String.iter unescapechar s;
      Buffer.contents buf

  let count_lines s =
    let n = ref 0 in
      String.iter
        (fun c -> if c = '\n' then incr n)
        s;
      !n


let print_token = function
	| ENDMARKER 					-> print_string "<end of file>"
	| NEWLINE 					-> print_newline () 
	| NAME id           				-> print_string "(ident : "; print_string id; print_string ")"
	| INTEGERLIT i					-> print_string "(int : "; print_int i; print_string ")"
	| STRINGLIT s					-> print_string "(string : "; print_string s; print_string ")"
	| INDENT					-> print_string "< indent >"
	| DEDENT					-> print_string "< dedent >"
	| CLASS						-> print_string "< class >"
	| DEF						-> print_string "< def >"
	| ELSE						-> print_string "< else >"
	| FOR						-> print_string "< for >"
	| IF						-> print_string "< if >"
	| IN						-> print_string "< in >"
	| PRINT						-> print_string "< print >"
	| RETURN					-> print_string "< return >"

	| ADD						-> print_string "< add >"
	| MINUS						-> print_string "< MINUS >"
	| TIMES						-> print_string "< times >"
	| DIV						-> print_string "< div >"
	| MOD						-> print_string "< mod >"
	| POWER						-> print_string "< power >"
	| EQ						-> print_string "< eq >"

	| ADDEQ						-> print_string "< addeq >"
	| SUBEQ						-> print_string "< MINUSeq >"
	| MULTEQ					-> print_string "< multeq >"
	| DIVEQ						-> print_string "< diveq >"
	| MODEQ						-> print_string "< modeq >"
	| POWEQ						-> print_string "< powerq >"

	| EQUAL						-> print_string "< equal >"
	| NOTEQ						-> print_string "< noteq >"
	| LT						-> print_string "< lt >"
	| GT						-> print_string "< gt >"
	| LEQ						-> print_string "< leq >"
	| GEQ						-> print_string "< geq >"
	| LPAREN					-> print_string "< lparen >"
	| RPAREN					-> print_string "< rparen >"
	| LBRACK					-> print_string "< lbraack >"
	| RBRACK					-> print_string "< rbrack >"
	| LBRACE					-> print_string "< lbrace >"
	| RBRACE					-> print_string "< rbrace >"
	| COLON						-> print_string "< colon >"
	| DOT						-> print_string "< dot >"
	| COMMA						-> print_string "< comma >"
	| _ 						-> print_string "unknown token"


}

(* epsilon *)
let e = ""

let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']
let comment = '#' [^ '\n' '\r']*

let digit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let nonzerodigit = ['1'-'9']
let longintpostfix = ['l' 'L']
let decimalinteger = nonzerodigit digit*
let octinteger = '0' octdigit+
let hexinteger = '0' ['x' 'X'] hexdigit+
let intpart = digit+
let fraction = '.' digit+
let pointfloat = intpart? fraction | intpart '.'
let exponent = ['e' 'E'] ['+' '-']? digit+
let exponentfloat = (intpart | pointfloat) exponent
let floatnumber = pointfloat | exponentfloat
let imagnumber = (floatnumber | intpart) ['j' 'J']

let stringprefix = ('u' | 'U')? ('r' | 'R')?
let escapeseq = '\\' _

let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let nonidchar = [^ 'a'-'z' 'A'-'Z' '0'-'9' '_']

rule token state = parse
  | e { let curr_offset = state.curr_offset in
        let last_offset = Stack.top state.offset_stack in
          if curr_offset < last_offset
          then (ignore (Stack.pop state.offset_stack); DEDENT)
          else if curr_offset > last_offset
          then (Stack.push curr_offset state.offset_stack; INDENT)
          else _token state lexbuf }

and _token state = parse
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
          _token state lexbuf }
  | '\\' newline whitespace*
      { let pos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <-
            { pos with
                pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + 1 };
          _token state lexbuf }

  | whitespace+
      { _token state lexbuf }

  (* keywords *)
  | identifier as id
      {	match id with
        | "class"    -> CLASS 
        | "continue" -> CONTINUE 
	| "True"     -> BOOLEANLIT(true)
	| "False"    -> BOOLEANLIT(false)
        | "def"      -> DEF 
        | "else"     -> ELSE
        | "for"      -> FOR 
        | "if"       -> IF 
        | "in"       -> IN 
        | "print"    -> PRINT  
        | "return"   -> RETURN
        | _          -> NAME (id) }

  (* symbols *)
  | "+="    { ADDEQ }
  | "-="    { SUBEQ  }
  | "*="    { MULTEQ }
  | "/="    { DIVEQ  }
  | "%="    { MODEQ  }
  | "**="   { POWEQ  }


  | "=="    { EQUAL }
  | "!="    { NOTEQ }
  | "<>"    { NOTEQ }
  | "<="    { LEQ }
  | ">="    { GEQ }
  | '<'     { LT }
  | '>'     { GT }

  | '='     { EQ }

  | "**"    { POWER }
  | '+'     { ADD }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }

  | '('     { ignore_nl state; LPAREN }
  | ')'     { aware_nl state; RPAREN }
  | '['     { ignore_nl state; LBRACK }
  | ']'     { aware_nl state; RBRACK }
  | '{'     { ignore_nl state; LBRACE }
  | '}'     { aware_nl state; RBRACE }
  | ':'     { COLON }
  | '.'     { DOT }
  | ','     { COMMA }

  (* literals *)
  | decimalinteger as n longintpostfix
      { LONGINT (int_of_string n) }
  | decimalinteger as n
      { INTEGERLIT (int_of_string n) }
  | octinteger as n longintpostfix
      { LONGINT (int_of_string ("0o" ^ n)) }
  | octinteger as n
      { INTEGERLIT (int_of_string ("0o" ^ n)) }
  | hexinteger as n longintpostfix
      { LONGINT (int_of_string n) }
  | hexinteger as n
      { INTEGERLIT (int_of_string n) }
  | '0' longintpostfix
      { LONGINT (0) }
  | '0'
      { INTEGERLIT (0) }

  | stringprefix '\''
      { sq_shortstrlit state 0 lexbuf }
  | stringprefix '"'
      { dq_shortstrlit state 0 lexbuf }
  | stringprefix "'''"
      { sq_longstrlit state 0 lexbuf }
  | stringprefix "\"\"\""
      { dq_longstrlit state 0 lexbuf }

  (* eof *)
  | eof { ENDMARKER }

and offset state = parse
  | e { }
  | ' '  { state.curr_offset <- state.curr_offset + 1; offset state lexbuf }
  | '\t' { state.curr_offset <- state.curr_offset + 8; offset state lexbuf }

and sq_shortstrlit state pos = parse
  | (([^ '\\' '\r' '\n' '\''] | escapeseq)* as s) '\'' { STRINGLIT (unescaped s) }

and sq_longstrlit state pos = shortest
| (([^ '\\'] | escapeseq)* as s) "'''"
    { let lines = count_lines s in
      let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_lnum = curpos.pos_lnum + lines };
        STRINGLIT (unescaped s) }

and dq_shortstrlit state pos = parse
  | (([^ '\\' '\r' '\n' '\"'] | escapeseq)* as s) '"' { STRINGLIT (unescaped s) }

and dq_longstrlit state pos = shortest
  | (([^ '\\'] | escapeseq)* as s) "\"\"\""
      { let lines = count_lines s in
        let curpos = lexbuf.lex_curr_p in
          lexbuf.lex_curr_p <- { curpos with pos_lnum = curpos.pos_lnum + lines };
          STRINGLIT (unescaped s) }

