type identifier = string

type modul = 
	| Module of stmt

and stmt =
	| Compare of stmt * comop * stmt
	| Str of string
	| Num of number 

and comop =
	| Eq

and number =
  | Int of int
  | LongInt of int
  | Float of float
  | Imag of string

let name_of_stmt = function 
	| Compare _ -> "Compare"

let context_of_expr = function
  | _                        -> None

let string_of_boolop = function
  | _                        -> None

let string_of_operator = function
  | _                        -> None

let string_of_unaryop = function
  | _                        -> None

let string_of_cmpop = function
  | _                        -> None


let string_of_number = function
  | Int (n)      -> string_of_int n

