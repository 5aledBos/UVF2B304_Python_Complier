(*
 * The full abstract grammar is available at:
 *   http://docs.python.org/library/ast.html#abstract-grammar
 *)

type identifier = string

type modul = 
	| Module of stmt
	| Interactive of stmt
	| Expression of expr

and 'a stmt =
  	| FunctionDef of identifier * arguments * stmt list * expr list
  	| ClassDef of identifier * expr list * stmt list * expr list 
	| Return of expr option
