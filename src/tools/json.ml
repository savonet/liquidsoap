type t = Json_base.t

let from_string ?(pos = []) ?(json5 = false) s =
  let parser = if json5 then Json_parser.json5 else Json_parser.json in
  let processor = MenhirLib.Convert.Simplified.traditional2revised parser in
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokenizer () =
    let token =
      if json5 then Json_lexer.json5_token lexbuf
      else Json_lexer.json_token lexbuf
    in
    let startp, endp = Sedlexing.lexing_positions lexbuf in
    (token, startp, endp)
  in
  try processor tokenizer with
    | Runtime_error.Runtime_error _ as exn ->
        let bt = Printexc.get_raw_backtrace () in
        Printexc.raise_with_backtrace exn bt
    | _ ->
        raise
          Runtime_error.(
            Runtime_error { kind = "json"; msg = "Parse error"; pos })

(* Special version of utf8 quoting that uses [Uchar.rep]
   when a character cannot be escaped. *)
let quote_utf8_string =
  let escape_char =
    let utf8_char_code s =
      try Utils.utf8_char_code s with _ -> Uchar.to_int Uchar.rep
    in
    Utils.escape_char ~escape_fun:(fun s ->
        Printf.sprintf "\\u%04X" (utf8_char_code s))
  in
  let next s i =
    try Utils.utf8_next s i with _ -> max (String.length s) (i + 1)
  in
  let escape_utf8_formatter =
    Utils.escape ~special_char:Utils.utf8_special_char ~escape_char ~next
  in
  fun s -> Printf.sprintf "\"%s\"" (Utils.escape_string escape_utf8_formatter s)

let rec to_string_compact ~json5 = function
  | `Null -> "null"
  | `String s -> Utils.quote_utf8_string s
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> (
      match classify_float f with
        | FP_infinite when json5 -> if f < 0. then "-Infinity" else "Infinity"
        | FP_nan when json5 -> if f < 0. then "-NaN" else "NaN"
        | FP_infinite | FP_nan ->
            raise
              Runtime_error.(
                Runtime_error
                  {
                    kind = "json";
                    msg =
                      "Infinite or Nan number cannot be represented in JSON. \
                       You might want to consider using the `json5` \
                       representation.";
                    pos = [];
                  })
        | _ ->
            let s = string_of_float f in
            let s = Printf.sprintf "%s" s in
            if s.[String.length s - 1] = '.' then Printf.sprintf "%s0" s else s)
  | `Tuple l ->
      "[" ^ String.concat "," (List.map (to_string_compact ~json5) l) ^ "]"
  | `Assoc l ->
      let l =
        List.map
          (fun (l, v) ->
            Printf.sprintf "\"%s\":%s" l (to_string_compact ~json5 v))
          l
      in
      Printf.sprintf "{%s}" (String.concat "," l)

let rec to_string_pp ~json5 f v =
  match v with
    | `Tuple l ->
        Format.fprintf f "@[[@;<1 1>@[";
        let rec aux = function
          | [] -> ()
          | [p] -> Format.fprintf f "%a" (to_string_pp ~json5) p
          | p :: l ->
              Format.fprintf f "%a,@;<1 0>" (to_string_pp ~json5) p;
              aux l
        in
        aux l;
        Format.fprintf f "@]@;<1 0>]@]"
    | `Assoc l ->
        Format.fprintf f "@{{@;<1 1>@[";
        let rec aux = function
          | [] -> ()
          | [(k, v)] ->
              Format.fprintf f "%s: %a" (Utils.quote_string k)
                (to_string_pp ~json5) v
          | (k, v) :: l ->
              Format.fprintf f "%s: %a,@;<1 0>" (Utils.quote_string k)
                (to_string_pp ~json5) v;
              aux l
        in
        aux l;
        Format.fprintf f "@]@;<1 0>}@]"
    | `Null | `String _ | `Int _ | `Float _ | `Bool _ ->
        Format.fprintf f "%s" (to_string_compact ~json5 v)

let to_string_pp ~json5 v =
  let b = Buffer.create 10 in
  let f = Format.formatter_of_buffer b in
  ignore (to_string_pp ~json5 f v);
  Format.pp_print_flush f ();
  Buffer.contents b

let to_string ?(compact = true) ?(json5 = false) v =
  if compact then to_string_compact ~json5 v else to_string_pp ~json5 v
