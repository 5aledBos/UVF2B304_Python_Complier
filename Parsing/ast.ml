(*
 * The full abstract grammar is available at:
 *   http://docs.python.org/library/ast.html#abstract-grammar
 *)

type identifier = string

type modul = 
	| Module of stmt list
	| Interactive of stmt list
	| Expression of expr list

and stmt =
  	| FunctionDef of identifier * arguments * stmt list
  	| ClassDef of identifier *stmt list  
	| Return of expr
  	| Delete of expr list
  	| Assign of expr list * expr
  	| AugAssign of expr * operator * expr

  	| Print of expr option * expr list * bool 
  	| For of expr * expr * stmt list * stmt list
  	| While of  expr * stmt list * stmt list 
  	| If of expr * stmt list * stmt list 
  	| With of expr * expr option * stmt list 

  	| Raise of expr option * expr option * expr option
  	| TryExcept of stmt list * excepthandler list * stmt list 
  	| TryFinally of stmt list * stmt list 
  	| Assert of expr * expr option
  	| Import of alias list
  	| ImportFrom of identifier * alias list * int option 
    	| Exec of expr * expr option * expr option
    	| Global of identifier list
  	| Expr of expr
    	| Pass  
  	| Break 
	| Continue 

and expr =
  | BoolOp of boolop * expr list
  | BinOp of expr * operator * expr
  | UnaryOp of unaryop * expr 
  | Lambda of arguments * expr
  | IfExp of expr * expr * expr
  | Dict of expr list * expr list
  | ListComp of expr * comprehension list
  | GeneratorExp of expr * comprehension list
  | Yield of expr option
  | Compare of expr * cmpop list * expr list
  | Call of expr * expr list * keyword list * expr option * expr option 
  | Repr of expr
  | Num of number
  | Str of string
  | Attribute of expr * identifier * expr_context
  | Subscript of expr * slice * expr_context
  | Name of identifier * expr_context
  | List of expr list * expr_context
  | Tuple of expr list * expr_context

and expr_context = 
	| Load 
	| Store 
	| Del 
	| AugLoad 
	| AugStore 
	| Param

and slice =
  | Ellipsis
  | Slice of expr option * expr option * expr option
  | ExtSlice of slice list
  | Index of expr

and boolop = 
	|And 
	| Or


and operator = 
	| Add 
	| Sub 
	| Mult 
	| Div 
	| Mod 
	| Pow 
	| LShift
    | RShift 
	| BitOr 
	| BitXor 
	| BitAnd 
	| FloorDiv

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

and comprehension = expr * expr * expr list 

and excepthandler = 
	| ExceptHandler of expr option * expr option * stmt list

and arguments = expr list * identifier option * identifier option * expr list 

and keyword =  identifier * expr

and alias = identifier * identifier option

and number =
  | Int of int
  | LongInt of int
  | Float of float
  | Imag of string

let name_of_mod = function
  | Module _      -> "Module"
  | Interactive _ -> "Interactive"
  | Expression _  -> "Expression"

