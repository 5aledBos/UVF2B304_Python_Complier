open PythonLexer
open Parser
open ErrorHandler
open Lexing
open Lexer_state
open Dump


 
let rec lexAllBuf lexbuf state =
let lex = (PythonLexer.token state) lexbuf in
match lex with
| ENDMARKER -> ()
(*| newline ->  print_string " "; print_newline (); lexAllBuf lexbuf *)
| _ -> print_token lex; print_string "-"; lexAllBuf lexbuf state

 
let compile file =
print_string ("File "^file^" is being treated!\n");
try
	let input_file = open_in file in
	let lexbuf = Lexing.from_channel input_file in
	try
		let state = Lexer_state.create () in 
		(*lexAllBuf lexbuf state;*)
		let ast = Parser.prog (PythonLexer.token state) lexbuf in
		close_in input_file;
		print_string "SUCCESS";
		print_newline();
		
	with
	| Errord (kind, debut, fin) ->
		close_in (input_file);
		report_error kind;
		print_position debut fin;
		print_newline ();
		close_in (input_file);
	| Parser.Error ->
		print_string "parsing error :";
		print_position (lexeme_start_p lexbuf) (lexeme_end_p lexbuf);
		print_newline ();
		print_string (Lexing.lexeme lexbuf);
		print_newline ();
with Sys_error s -> print_endline ("Can’t find file ’" ^ file ^ "’")
let _ = Arg.parse [] compile ""
