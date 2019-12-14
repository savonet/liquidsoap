type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of t list
  | `Null
  | `String of string ]

exception Error

let lexer = Genlex.make_lexer ["{"; "}"; "["; "]"; ","; ":"]

let rec parse stream =
  match Stream.next stream with
    | Genlex.Ident "true" ->
        `Bool true
    | Genlex.Ident "false" ->
        `Bool false
    | Genlex.Ident "null" ->
        `Null
    | Genlex.String s | Genlex.Ident s ->
        `String s
    | Genlex.Char c ->
        `String (String.make 1 c)
    | Genlex.Float f ->
        `Float f
    | Genlex.Int n ->
        `Int n
    | Genlex.Kwd "{" ->
        `Assoc (parse_assoc stream)
    | Genlex.Kwd "[" ->
        `List (parse_list stream)
    | _ ->
        raise Error

and parse_assoc stream =
  match Stream.next stream with
    | Genlex.String l | Genlex.Ident l ->
        if Stream.next stream <> Genlex.Kwd ":" then raise Error ;
        let v = parse stream in
        let k = Stream.next stream in
        if k = Genlex.Kwd "," then (l, v) :: parse_assoc stream
        else if k = Genlex.Kwd "}" then [(l, v)]
        else raise Error
    | Genlex.Kwd "}" ->
        []
    | _ ->
        raise Error

and parse_list stream =
  match Stream.peek stream with
    | Some (Genlex.Kwd "]") ->
        Stream.junk stream ; []
    | Some _ ->
        let v = parse stream in
        let k = Stream.next stream in
        if k = Genlex.Kwd "," then v :: parse_list stream
        else if k = Genlex.Kwd "]" then [v]
        else raise Error
    | None ->
        raise Error

let from_string s =
  let lexer = lexer (Stream.of_string s) in
  parse lexer

let escape_string s = String.escaped s

let to_string (j : t) =
  let blank indent = String.make (2 * indent) ' ' in
  let rec aux indent = function
    | `Bool b ->
        if b then "true" else "false"
    | `Int n ->
        string_of_int n
    | `Float x ->
        string_of_float x
    | `String s ->
        "\"" ^ escape_string s ^ "\""
    | `Null ->
        "null"
    | `List l ->
        let l =
          List.map (fun j -> blank (indent + 1) ^ aux (indent + 1) j) l
        in
        let l = String.concat ",\n" l in
        "[\n" ^ l ^ "\n" ^ blank indent ^ "]"
    | `Assoc l ->
        let l =
          List.map
            (fun (k, v) ->
              blank (indent + 1) ^ "\"" ^ k ^ "\": " ^ aux (indent + 1) v)
            l
        in
        let l = String.concat ",\n" l in
        "{\n" ^ l ^ "\n" ^ blank indent ^ "}"
  in
  aux 0 j
