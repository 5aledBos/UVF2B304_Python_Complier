open Ast
open Format

exception Error of string
let error s = raise(Error s)



type value =
  | Vnone
  | Vbool of bool
  | Vint of int
  | Vstring of string
  | Vlist of value array

let is_false = function
  | Vnone
  | Vbool false
  | Vstring ""
  | Vlist [||] -> true
  | Vint n -> n = 0
  | _ -> false

let is_true v = not (is_false v)

let rec print_value = function
  | Vnone -> printf "None"
  | Vbool true -> printf "True"
  | Vbool false -> printf "False"
  | Vint n -> printf "%d" n
  | Vstring s -> printf "%s" s
  | Vlist a ->
    let n = Array.length a in
    printf "[";
    for i = 0 to n-1 do print_value a.(i); if i < n-1 then printf ", " done;
    printf "]"

let get_value = function
	| Vstring s -> s
	| Vint s -> string_of_int s
	| _ -> "Can't get Value"

let rec compare_list a1 n1 a2 n2 i =
  if i = n1 && i = n2 then 0
  else if i = n1 then -1
  else if i = n2 then 1
  else let c = compare a1.(i) a2.(i) in
       if c <> 0 then c else compare_list a1 n1 a2 n2 (i + 1)

let rec compare_value v1 v2 = match v1, v2 with
  | Vlist a1, Vlist a2 ->
    compare_list a1 (Array.length a1) a2 (Array.length a2) 0
  | _ -> compare v1 v2

let binop_value op v1 v2 = 
	match op, v1, v2 with
  	| Add, Vint n1, Vint n2 -> Vint (n1+n2)
  	| Add, Vlist l1, Vlist l2 -> Vlist (Array.append l1 l2)
	| Add, Vstring s1, Vstring s2 -> Vstring (s1 ^ s2)
	| Add, Vstring s, Vint n -> Vstring (s^(string_of_int n))
	| Add, Vint n, Vstring s -> Vstring ((string_of_int n)^s)
	| Sub, Vint n1, Vint n2 -> Vint ( n1 - n2 )
	| Mult, Vint n1, Vint n2 -> Vint ( n1 * n2 )
	| Div, Vint n1, Vint n2 -> Vint( n1 / n2 )
	| Mod, Vint n1, Vint n2 -> Vint( n1 mod n2)
	| Pow, Vint n1, Vint n2 -> Vint( int_of_float ((float_of_int n1) ** (float_of_int n2)) )
	| (Div | Mod), Vint _, Vint 0 -> error "division by zero"
    	| _ -> error "unsupported bin operation type"


let comop_value op v1 v2 =
	match op, v1, v2 with
 	| Eq, _, _ -> Vbool (compare_value v1 v2 = 0)
	| NotEq, _, _ -> Vbool (compare_value v1 v2 <> 0)
	| Lt, _, _ -> Vbool (compare_value v1 v2 < 0)
	| LtE, _, _ -> Vbool (compare_value v1 v2 <= 0)
	| Gt, _, _ -> Vbool (compare_value v1 v2 > 0)
	| GtE, _, _ -> Vbool (compare_value v1 v2 >= 0)
	| _ -> error "unsupported comp operation type"

exception Return of value
type ctx = (string, value) Hashtbl.t

type clazz = {
	name : string;
	attributes : ctx;
	methods : (string, identifier list*stmt list) Hashtbl.t;
	mutable objects: string list;
}

type global_ctx = (string, clazz) Hashtbl.t


type object_ = {
	objName: string;
	className: string;
	objAttributes: ctx;
}

let global_objects = (Hashtbl.create 16 : (string, object_) Hashtbl.t)


let classes = (Hashtbl.create 16 : (string, clazz) Hashtbl.t)


	


let rec expr global_ctx clazz = function
	| Str(s) -> Vstring s
	| Num(Int s) -> Vint s
	| Bool(s) -> Vbool s
	| BinOp(e1, op , e2) -> binop_value op (expr global_ctx clazz e1) (expr global_ctx clazz e2)
	| Compare(e1, op, e2) -> 
				comop_value op (expr global_ctx clazz e1) (expr global_ctx clazz e2)
	| Call(className, functionName, arguments) -> 
			 	if className <> functionName
			 	then 
					if Hashtbl.mem clazz.attributes className = true then (* call on an object : className here is the name of the object *)
						let objName = Hashtbl.find clazz.attributes className in
						let obj = Hashtbl.find global_objects (get_value objName) in
						let targetClass = Hashtbl.find global_ctx obj.className in
						if Hashtbl.mem targetClass.methods functionName = true then
							let args, body = Hashtbl.find targetClass.methods functionName in
							if List.length args <> List.length arguments then error ("wrong number of arguments passed to the function : "^functionName^" of the class : "^obj.className);
					 		List.iter2 (fun x e -> Hashtbl.replace obj.objAttributes x (expr global_ctx clazz e)) args arguments;
							begin try List.iter (obj_stmt global_ctx obj) body; Vnone with Return v -> v end
							(** TODO :: delete function local attributes from class contexte after running statements **)	
						else
							error ("Unbound Function : "^functionName^" with class : "^obj.className)
					else error ("Unknown attribute")
			 	else if Hashtbl.mem clazz.methods className = true then (* global function call *)
					 let args, body = Hashtbl.find clazz.methods className in
					 if List.length args <> List.length arguments then error ("wrong number of arguments passed to the function : "^className);
					 List.iter2 (fun x e -> Hashtbl.add clazz.attributes x (expr global_ctx clazz e)) args arguments;
				  	 begin try List.iter (stmt global_ctx clazz) body; Vnone with Return v -> v end	
				else if Hashtbl.mem global_ctx className = true then (* Object creation *)
					let targetClass = Hashtbl.find global_ctx className in
					let nbrOfObjects = (List.length targetClass.objects) + 1 in
					if Hashtbl.mem targetClass.methods "__init__" <> true then error("No constructor found");
					let args, body = Hashtbl.find targetClass.methods "__init__" in 
					if List.length args <> List.length arguments then error ("wrong number of arguments passed to the constructor of the class : "^className);					
					let o = {
						objName = className^"-obj-"^(string_of_int nbrOfObjects);
						className = className;
						objAttributes = Hashtbl.create 16;
					} in
					Hashtbl.replace global_objects o.objName o;
					targetClass.objects <- targetClass.objects@[o.objName];
			 		List.iter2 (fun x e -> Hashtbl.replace o.objAttributes x (expr global_ctx clazz e)) args arguments;
					begin try List.iter (obj_stmt global_ctx o) body; Vstring o.objName with Return v -> v end
					
 				else error ("function or Constructor : "^className^" is not a member of "^clazz.name)

	| Name(s,param) -> 
				let sl = String.split_on_char '.' s in
				if Hashtbl.mem clazz.attributes s = true
				then Hashtbl.find clazz.attributes s
				else if List.length sl = 2 then (* object.attribute *)
					error("use accessors to access class attributes")	
				else
					error ("attribute "^ s ^" not member of : "^clazz.name)
	| List(e, param) ->
				Vlist (Array.of_list (List.map (expr global_ctx clazz) e))
	| _ -> Vnone

and obj_expr global_ctx obj = function
	| Str(s) -> Vstring s
	| Num(Int s) -> Vint s
	| Bool(s) -> Vbool s
	| Name(s,param) -> 
			if Hashtbl.mem obj.objAttributes s = true
			then 
				let v = Hashtbl.find obj.objAttributes s in 
				Hashtbl.find obj.objAttributes s
			else error("attribute "^ s ^" not member of : "^obj.className)
	| BinOp(e1, op , e2) -> binop_value op (obj_expr global_ctx obj e1) (obj_expr global_ctx obj e2)
	| Compare(e1, op, e2) -> 
				comop_value op (obj_expr global_ctx obj e1) (obj_expr global_ctx obj e2)
	| List(e, param) ->
				Vlist (Array.of_list (List.map (obj_expr global_ctx obj) e))
	| Call(className, functionName, arguments) -> Vint 1 (** TODO : Use Objects Everywhere !! Clean Code !! And that's it :) **)
	| _ -> Vnone



and obj_stmt global_ctx obj = function
	| Assign(targets, value) -> Hashtbl.replace obj.objAttributes targets (obj_expr global_ctx obj value)
	| If(e, s1, s2) -> 
			if is_true (obj_expr global_ctx obj e) then List.iter (obj_stmt global_ctx obj) s1 else List.iter (obj_stmt global_ctx obj) s2
	| For(i, e, s) -> begin match obj_expr global_ctx obj e with
				 | Vlist l -> Array.iter (fun v -> Hashtbl.replace obj.objAttributes i v; List.iter (obj_stmt global_ctx obj) s) l
				 | _ -> error "This expression should be a list"
				end
	| Return(e) -> raise( Return (obj_expr global_ctx obj e) )
	| Print(e) -> print_value (obj_expr global_ctx obj e); printf "@."
	| _ -> error "this stmt is not implemented inside Object scope";
 
and stmt global_ctx clazz = function
	| Assign(targets, value) -> Hashtbl.replace clazz.attributes targets (expr global_ctx clazz value)
				
	| FunctionDef(identifier, arguments, body) -> 
				Hashtbl.add clazz.methods identifier (arguments,body);
				Hashtbl.replace global_ctx clazz.name clazz;
	| Print(e) -> print_value (expr global_ctx clazz e); printf "@."
	| Return(e) -> raise( Return (expr global_ctx clazz e) )
	| If(e, s1, s2) -> 
			if is_true (expr global_ctx clazz e) then List.iter (stmt global_ctx clazz) s1 else List.iter (stmt global_ctx clazz) s2
	| For(i, e, s) -> begin match expr global_ctx clazz e with
				 | Vlist l -> Array.iter (fun v -> Hashtbl.replace clazz.attributes i v; List.iter (stmt global_ctx clazz) s) l
				 | _ -> error "This expression should be a list"
				end	
	| _ -> print_string "not implemented !";print_newline()


let add_class global_ctx name body =
	let c = {
		name = name;
		attributes = Hashtbl.create 16;
		methods = Hashtbl.create 16;
		objects = [];	
	} in
	Hashtbl.replace global_ctx name c;
	List.iter (stmt global_ctx c) body

let f body global_ctx = 
	match body with
	 | ClassDef(name, body) -> 
				add_class global_ctx name body;
	 | _ -> stmt global_ctx (Hashtbl.find global_ctx "Module")  body
	 




let interp cl = 
	let modul = {
		name = "Module";
		attributes = Hashtbl.create 16;
		methods = Hashtbl.create 16;	
		objects = [];
	} in
	let global_contexte : global_ctx = Hashtbl.create 16 in
	Hashtbl.add global_contexte "Module" modul;
	List.iter
		(fun(x) -> f x global_contexte) cl






