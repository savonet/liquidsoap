
type parameter_type =
  | Bool_t
  | Int_t
  | String_t
  | Float_t
  | Source_t
  | List_t    of parameter_type
  | Product_t of parameter_type*parameter_type

type parameter =
  | Bool    of bool
  | Int     of int
  | String  of string
  | Float   of float
  | Source  of Types.source
  | List    of parameter list
  | Product of parameter*parameter

let rec type_to_string = function
  | Bool_t -> "bool"
  | Int_t -> "int"
  | Float_t -> "float"
  | String_t -> "string"
  | Source_t -> "source"
  | List_t p -> Printf.sprintf "%s list" (type_to_string p)
  | Product_t (a,b) ->
      Printf.sprintf "(%s*%s)" (type_to_string a) (type_to_string b)

let rec parameter_to_string = function
  | Bool i -> string_of_bool i
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> "\""^s^"\""
  | Source s -> "[source]"
  | List l ->
      "["^(List.fold_left (fun t s -> t^";"^(parameter_to_string s)) "" l)^"]"
  | Product (a,b) ->
      Printf.sprintf "(%s,%s)" (parameter_to_string a) (parameter_to_string b)

(** Check the type of an object. *)
let rec has_type t = function
  | Bool _ -> t = Bool_t
  | Int _ -> t = Int_t
  | String _ -> t = String_t
  | Float _ -> t = Float_t
  | Source _ -> t = Source_t
  | List [] ->
      begin
        match t with
          | List_t p -> true
          | _ -> false
      end
  | List (hd::tl) ->
      begin
        match t with
          | List_t p -> has_type p hd && has_type t (List tl)
          | _ -> false
      end
  | Product (a,b) ->
      begin
        match t with
          | Product_t (t1,t2) ->
              has_type t1 a && has_type t2 b
          | _ -> false
      end

(** Get the type of an object. No inference is done, so type of list values
  * is not guessed. *)
let rec get_type = function
  | Bool _ -> "bool"
  | Int _ -> "int"
  | String _ -> "string"
  | Float _ -> "float"
  | Source _ -> "source"
  | Product (a,b) -> Printf.sprintf "(%s*%s)" (get_type a) (get_type b)
  | List [] -> "(?) list"
  | List (a::_) -> Printf.sprintf "(%s) list" (get_type a)

(** Prototype of a function *)

type default_value = parameter option
type documentation = string option
type prototype  = (string*parameter_type*default_value*documentation) list

exception Multiple_definitions of string
exception Missing_definition of string
exception Wrong_type of (string*string*parameter_type)
exception Wrong_label of string

exception Unbound of string

exception Invalid_value of string*string

(** User specified parameters, which is a candidate instantiation *)

type parameters = (string*parameter) list

(** Internal structure allowing easy validation
  * of parameters against prototyope *)

type dus = Default of parameter | Unset | Set of parameter
type internal = (string,parameter_type*dus) Hashtbl.t

let dus_of_opt = function
  | None -> Unset
  | Some v -> Default v

let internal_of_prototype p =
  let b = Hashtbl.create (List.length p) in
    List.iter (fun (l,t,d,_) ->
		 if Hashtbl.mem b l then raise (Multiple_definitions l)
		 else Hashtbl.add b l (t,(dus_of_opt d))) p ;
    b

let add_binding b k v =
  try
    let t,dus = Hashtbl.find b k in
      if not (has_type t v) then raise (Wrong_type (k,(get_type v),t)) ;
      ( match dus with
	  | Default _
	  | Unset -> Hashtbl.replace b k (t,(Set v))
	  | Set vv -> raise (Multiple_definitions k) )
  with
    | Not_found -> raise (Wrong_label k)

let output_bindings b =
  let o = Hashtbl.create (Hashtbl.length b) in
    Hashtbl.iter (fun k (_,dus) ->
		    match dus with
		      | Default v
		      | Set v -> Hashtbl.add o k v
		      | Unset -> raise (Missing_definition k)) b ;
    o

(** Now we check parameters against (internal representation of) prototype *)

let check b param =
  let b = Hashtbl.copy b in
    List.iter (fun (l,v) -> add_binding b l v) param ;
    output_bindings b

(** A few extractors *)

let to_string = function
  | String s -> s
  | _ -> assert false

let to_float = function
  | Float s -> s
  | _ -> assert false

let to_source = function
  | Source s -> s
  | _ -> assert false

let to_int = function
  | Int s -> s
  | _ -> assert false

let to_list = function
  | List l -> l
  | _ -> assert false

let to_string_list l = List.map to_string (to_list l)
let to_int_list l = List.map to_int (to_list l)
let to_source_list l = List.map to_source (to_list l)

(** Documentation management *)

let doc_of_prototype_item t d doc =
  let doc = match doc with None -> "(no doc)" | Some d -> d in    
  let item = new Doc.item 3 doc in
    item#add_subsection "type" (Doc.trivial (type_to_string t)) ; 
    item#add_subsection "default"
      (match d with
	 | None -> Doc.trivial "None"
	 | Some d -> Doc.trivial (parameter_to_string d)) ;
    item

let to_doc main_doc proto =
  let item = new Doc.item (List.length proto) main_doc in
    List.iter
      (fun (l,t,d,doc) ->
	 item#add_subsection l (doc_of_prototype_item t d doc)) proto ;
    item

(** Where to store all the symbols for the language *)

let operators : (parameters -> Types.source) Plug.plug
  = Plug.create ~doc:"Stream generators." ~size:50 "operators"
