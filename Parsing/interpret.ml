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
	| _ -> "not string"

let binop_value op v1 v2 = 
	match op, v1, v2 with
  	| Add, Vint n1, Vint n2 -> Vint (n1+n2)
    	| _ -> error "unsupported operand types"

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
	| BinOp(e1, Add, e2) -> binop_value Add (expr global_ctx clazz e1) (expr global_ctx clazz e2)
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
					 		List.iter2 (fun x e -> Hashtbl.replace targetClass.attributes x (expr global_ctx clazz e)) args arguments;
							begin try List.iter (obj_stmt global_ctx obj) body; Vnone with Return v -> v end	
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
					let o = {
						objName = className^"-obj-"^(string_of_int nbrOfObjects);
						className = className;
						objAttributes = Hashtbl.copy targetClass.attributes;
					} in
					Hashtbl.replace global_objects o.objName o;
					targetClass.objects <- targetClass.objects@[o.objName];
					Vstring o.objName
 				else error ("function or Constructor : "^className^" is not a member of "^clazz.name)

	| Name(s,param) -> 
				let sl = String.split_on_char '.' s in
				if Hashtbl.mem clazz.attributes s = true
				then Hashtbl.find clazz.attributes s
				else if List.length sl = 2 then (* object.attribute *)
					error("use accessors to access class attributes")	
				else
					error ("attribute "^ s ^" not member of : "^clazz.name)
	| _ -> Vnone

and obj_expr global_ctx obj = function
	| Name(s,param) -> 
			if Hashtbl.mem obj.objAttributes s = true
			then Hashtbl.find obj.objAttributes s
			else error("attribute "^ s ^" not member of : "^obj.className)


and obj_stmt global_ctx obj = function
	| Assign(targets, value) -> Hashtbl.replace obj.objAttributes targets (expr global_ctx (Hashtbl.find global_ctx obj.className) value)
	| Return(e) -> raise( Return (obj_expr global_ctx obj e) )
 
and stmt global_ctx clazz = function
	| Assign(targets, value) -> Hashtbl.replace clazz.attributes targets (expr global_ctx clazz value)
				
	| FunctionDef(identifier, arguments, body) -> 
				Hashtbl.add clazz.methods identifier (arguments,body);
				Hashtbl.replace global_ctx clazz.name clazz;
	| Print(e) -> print_value (expr global_ctx clazz e); printf "@."
	| Return(e) -> raise( Return (expr global_ctx clazz e) )
				
	| _ -> print_string "not implemented yet !";print_newline()


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






