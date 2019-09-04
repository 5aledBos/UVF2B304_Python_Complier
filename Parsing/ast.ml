(*
 * The full abstract grammar is available at:
 *   http://docs.python.org/library/ast.html#abstract-grammar
 *)

type identifier = string

type modul = 
	| Module of stmt list (* not used because import is not implemented ; the interpreter takes a list of statement as an argument*)

and stmt =
  	| FunctionDef of identifier * identifier list * stmt list
  	| ClassDef of identifier *stmt list  
	| Return of expr
  	| Assign of identifier * expr
  	| AugAssign of expr * operator * expr

  	| Print of expr
  	| For of identifier * expr * stmt list
  	| While of  expr * stmt list * stmt list 
  	| If of expr * stmt list * stmt list 

  	| Expr of expr

and expr =
  | BoolOp of boolop * expr list
  | BinOp of expr * operator * expr
  | UnaryOp of unaryop * expr 
  | Dict of expr list * expr list
  | Compare of expr * cmpop * expr
  | Call of identifier * identifier * arguments
  | Num of number
  | Str of string
  | Bool of bool
  | Name of identifier * expr_context
  | List of expr list * expr_context

and expr_context = 
	| Load 
	| Store 
	| Del 
	| AugLoad 
	| AugStore 
	| Param


and boolop = 
	| And 
	| Or


and operator = 
	| Add 
	| Sub 
	| Mult 
	| Div 
	| Mod 
	| Pow 

and unaryop = 
	| Invert 
	| Not 
	| UAdd 
	| USub

and cmpop = 
	| Eq 
	| NotEq 
	| Lt 
	| LtE 
	| Gt 
	| GtE 
	| Is 
	| IsNot 
	| In 
	| NotIn



and excepthandler = 
	| ExceptHandler of expr option * expr option * stmt list

and arguments = expr list

and number =
  | Int of int
  | LongInt of int
  | Float of float
  | Imag of string

let name_of_mod = function
  | Module _      -> "Module"

