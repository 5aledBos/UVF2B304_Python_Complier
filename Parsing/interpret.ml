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

exception Return of value
type ctx = (string, value) Hashtbl.t

type clazz = {
	name : string;
	attributes : ctx;
	methods : (string, stmt list) Hashtbl.t;
	objects: string list;
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
	| Call(className, functionName, arguments) -> 

			 	if className <> functionName
				
			 	then 
					if Hashtbl.mem clazz.attributes className = true then
						let objName = Hashtbl.find clazz.attributes className in
						let obj = Hashtbl.find global_objects (get_value objName) in
						let targetClass = Hashtbl.find global_ctx obj.className in
						if Hashtbl.mem targetClass.methods functionName = true then
							let body = Hashtbl.find targetClass.methods functionName in
							begin try List.iter (stmt global_ctx targetClass) body; Vnone with Return v -> v end	
						else
							error ("Unbound Function : "^functionName^" with class : "^obj.className)
					else error ("Unknown attribute")
			 	else if Hashtbl.mem clazz.methods className = true then
				 	 let body = Hashtbl.find clazz.methods className in
				  	 begin try List.iter (stmt global_ctx clazz) body; Vnone with Return v -> v end	
				else if Hashtbl.mem global_ctx className = true then
					let targetClass = Hashtbl.find global_ctx className in
					let nbrOfObjects = (List.length targetClass.objects) + 1 in
					let o = {
						objName = className^"-obj-"^(string_of_int nbrOfObjects);
						className = className;
						objAttributes = Hashtbl.copy targetClass.attributes;
					} in
					Hashtbl.replace global_objects o.objName o;
					List.append targetClass.objects [o.objName];
					Vstring o.objName
 				else error ("function or Constructor : "^className^" is not a member of "^clazz.name)

	| Name(s,param) -> 
				let sl = String.split_on_char '.' s in
				if Hashtbl.mem clazz.attributes s = true
				then Hashtbl.find clazz.attributes s
				else if List.length sl = 2 then
					if Hashtbl.mem clazz.attributes (List.hd sl) <> true 
					then error("Object "^(List.hd sl)^" unknown")
					else
						let objName = Hashtbl.find clazz.attributes (List.hd sl) in
						let obj = Hashtbl.find global_objects (get_value objName) in
						if Hashtbl.mem obj.objAttributes (List.nth sl 1) <> true 
						then error("attribute not member of : "^obj.className)
						else
							Hashtbl.find obj.objAttributes (List.nth sl 1)		
				else
					error ("attribute not member of : "^clazz.name)
	| _ -> Vnone



and stmt global_ctx clazz = function
	| Assign(targets, value) -> Hashtbl.replace clazz.attributes targets (expr global_ctx clazz value)
				
	| FunctionDef(identifier, arguments, body) -> 
				Hashtbl.add clazz.methods identifier body;
				Hashtbl.replace global_ctx clazz.name clazz;
	| Print(e) -> print_value (expr global_ctx clazz e); printf "@."
				
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
				print_newline()
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






