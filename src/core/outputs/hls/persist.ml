type segment = {
  position : int;
  discontinuous : bool;
  discontinuity : int;
  forced : bool;
  codecs : string option;
  init : string option;
  name : string;
  tags : string list;
  duration : float;
}

type variant = {
  name : string;
  next_position : int;
  next_init_position : int;
  discontinuity_count : int;
  window_start : int;
  pending_tags : string list;
  segments : segment list;
}

let strings l = `Tuple (List.map (fun s -> `String s) l)
let optional key = function Some v -> [(key, `String v)] | None -> []

let json_of_segment s =
  `Assoc
    ([
       ("position", `Int s.position);
       ("discontinuous", `Bool s.discontinuous);
       ("discontinuity", `Int s.discontinuity);
       ("forced", `Bool s.forced);
     ]
    @ optional "codecs" s.codecs @ optional "init" s.init
    @ [
        ("name", `String s.name);
        ("tags", strings s.tags);
        ("duration", `Float s.duration);
      ])

let json_of_variant v =
  `Assoc
    [
      ("name", `String v.name);
      ("next_position", `Int v.next_position);
      ("next_init_position", `Int v.next_init_position);
      ("discontinuity_count", `Int v.discontinuity_count);
      ("window_start", `Int v.window_start);
      ("pending_tags", strings v.pending_tags);
      ("segments", `Tuple (List.map json_of_segment v.segments));
    ]

let to_string variants =
  Json.to_string
    (`Assoc [("variants", `Tuple (List.map json_of_variant variants))])

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun s -> raise (Invalid s)) fmt

let field key = function
  | `Assoc l -> (
      match List.assoc_opt key l with
        | Some v -> v
        | None -> invalid "missing key %S" key)
  | _ -> invalid "expected an object around %S" key

let field_opt key = function `Assoc l -> List.assoc_opt key l | _ -> None

let to_int key = function
  | `Int n when n >= 0 -> n
  | _ -> invalid "%S: expected a non-negative integer" key

let to_bool key = function
  | `Bool b -> b
  | _ -> invalid "%S: expected a boolean" key

let to_str key = function
  | `String s -> s
  | _ -> invalid "%S: expected a string" key

let to_list key = function
  | `Tuple l -> l
  | _ -> invalid "%S: expected an array" key

let int key j = to_int key (field key j)
let bool key j = to_bool key (field key j)
let str key j = to_str key (field key j)
let str_list key j = List.map (to_str key) (to_list key (field key j))
let str_opt key j = Option.map (to_str key) (field_opt key j)

let float key j =
  match field key j with
    | `Float f when f >= 0. -> f
    | `Int n when n >= 0 -> float_of_int n
    | _ -> invalid "%S: expected a non-negative number" key

let segment_of_json j =
  {
    position = int "position" j;
    discontinuous = bool "discontinuous" j;
    discontinuity = int "discontinuity" j;
    forced = bool "forced" j;
    codecs = str_opt "codecs" j;
    init = str_opt "init" j;
    name = str "name" j;
    tags = str_list "tags" j;
    duration = float "duration" j;
  }

let variant_of_json j =
  {
    name = str "name" j;
    next_position = int "next_position" j;
    next_init_position = int "next_init_position" j;
    discontinuity_count = int "discontinuity_count" j;
    window_start = int "window_start" j;
    pending_tags = str_list "pending_tags" j;
    segments =
      List.map segment_of_json (to_list "segments" (field "segments" j));
  }

let rec increasing cmp = function
  | a :: (b :: _ as rest) -> cmp a b && increasing cmp rest
  | _ -> true

let check_variant ~valid_name v =
  if
    not
      (increasing (fun (a : segment) b -> a.position < b.position) v.segments
      && List.for_all
           (fun (s : segment) -> s.position < v.next_position)
           v.segments)
  then invalid "variant %S: positions not increasing below next_position" v.name;
  if
    not
      (increasing
         (fun (a : segment) b -> a.discontinuity <= b.discontinuity)
         v.segments
      && List.for_all
           (fun (s : segment) -> s.discontinuity <= v.discontinuity_count)
           v.segments)
  then invalid "variant %S: inconsistent discontinuity values" v.name;
  if v.window_start > v.next_position then
    invalid "variant %S: window_start after next_position" v.name;
  List.iter
    (fun (s : segment) ->
      List.iter
        (fun name ->
          if not (valid_name name) then
            invalid "variant %S: invalid file name %S" v.name name)
        (s.name :: Option.to_list s.init))
    v.segments

let of_string ~names ~valid_name s =
  try
    let json =
      try Json.from_string s
      with exn -> invalid "not valid JSON: %s" (Printexc.to_string exn)
    in
    let variants =
      List.map variant_of_json (to_list "variants" (field "variants" json))
    in
    let saved =
      List.sort compare (List.map (fun (v : variant) -> v.name) variants)
    in
    if saved <> List.sort compare names then
      invalid "the variant names differ from the output's";
    List.iter (check_variant ~valid_name) variants;
    Ok variants
  with Invalid reason -> Error reason
