(* errors handling *)
open Lexing

exception Eof

type error =
	| Illegal_character of char
	| Illegal_float of string

exception Errord of (error * position * position)

let raise_error err lexbuf =
		raise ( Errord(err, lexeme_start_p lexbuf, lexeme_end_p lexbuf) )

(* Les erreurs. *)
let report_error = function
	| Illegal_character c ->
		print_string "Illegal character ’";
		print_char c;
		print_string "’ "
	| Illegal_float nb ->
		print_string "The float ";
		print_string nb;
		print_string " is illegal "

let print_position debut fin =
	if (debut.pos_lnum = fin.pos_lnum) then
		begin
			print_string "line ";
			print_int debut.pos_lnum;
			print_string " characters ";
			print_int (debut.pos_cnum - debut.pos_bol);
			print_string "-";
			print_int (fin.pos_cnum - fin.pos_bol)
		end
	else
		begin
			print_string "from line ";
			print_int debut.pos_lnum;
			print_string " character ";
			print_int (debut.pos_cnum - debut.pos_bol);
			print_string " to line ";
			print_int fin.pos_lnum;
			print_string " character ";
			print_int (fin.pos_cnum - fin.pos_bol)
		end
