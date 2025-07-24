include Base

exception Infinite
exception NaN

module Json_string = String
module String = Stdlib.String

let string_of_float f =
  let s = string_of_float f in
  if s.[String.length s - 1] = '.' then s ^ "0" else s

let json5_processor =
  MenhirLib.Convert.Simplified.traditional2revised Parser.json5

let json_processor =
  MenhirLib.Convert.Simplified.traditional2revised Parser.json

let from_string ?(json5 = false) s =
  let processor = if json5 then json5_processor else json_processor in
  let lexbuf =
    let gen =
      let pos = ref (-1) in
      let len = String.length s in
      fun () ->
        incr pos;
        if !pos < len then Some s.[!pos] else None
    in
    Sedlexing.Utf8.from_gen gen
  in
  let tokenizer () =
    let token =
      if json5 then Lexer.json5_token lexbuf else Lexer.json_token lexbuf
    in
    let startp, endp = Sedlexing.lexing_bytes_positions lexbuf in
    (token, startp, endp)
  in
  processor tokenizer

(* Special version of utf8 quoting that uses [Uchar.rep]
   when a character cannot be escaped and fallback to
   ascii escaping with json escaping patterns.. *)
let quote_string =
  let escape_char =
    let utf8_char_code s pos len =
      try Json_string.utf8_char_code s pos len
      with _ -> Uchar.to_int Uchar.rep
    in
    Json_string.escape_char ~escape_fun:(fun s pos len ->
        Printf.sprintf "\\u%04X" (utf8_char_code s pos len))
  in
  let escape_formatter ~next special_char =
    Json_string.escape
      ~special_char:(fun s pos len ->
        if s.[pos] = '\'' && len = 1 then false else special_char s pos len)
      ~escape_char ~next
  in
  let escape_utf8_formatter =
    escape_formatter ~next:Json_string.utf8_next Json_string.utf8_special_char
  in
  let escape_ascii_formatter =
    escape_formatter ~next:Json_string.ascii_next Json_string.ascii_special_char
  in
  fun s ->
    Printf.sprintf "\"%s\""
      (try Json_string.escape_string escape_utf8_formatter s
       with _ -> Json_string.escape_string escape_ascii_formatter s)

let rec to_string_compact ~json5 = function
  | `Null -> "null"
  | `String s -> quote_string s
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> (
      match classify_float f with
        | FP_infinite when json5 -> if f < 0. then "-Infinity" else "Infinity"
        | FP_nan when json5 -> if f < 0. then "-NaN" else "NaN"
        | FP_infinite -> raise Infinite
        | FP_nan -> raise NaN
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

let pp_list sep ppx f l =
  let pp_sep f () = Format.fprintf f "%s@ " sep in
  Format.pp_print_list ~pp_sep ppx f l

let rec to_string_pp ~json5 f v =
  match v with
    | `Tuple [] -> Format.fprintf f "[]"
    | `Tuple l ->
        Format.fprintf f "[@;<1 0>%a@;<1 -2>]"
          (pp_list "," (to_string_pp ~json5))
          l
    | `Assoc [] -> Format.fprintf f "{}"
    | `Assoc l ->
        let format_field f (k, v) =
          Format.fprintf f "@[<hv2>%s: %a@]"
            (Json_string.quote_string k)
            (to_string_pp ~json5) v
        in
        Format.fprintf f "{@;<1 0>%a@;<1 -2>}" (pp_list "," format_field) l
    | `Null | `String _ | `Int _ | `Float _ | `Bool _ ->
        Format.fprintf f "%s" (to_string_compact ~json5 v)

let to_string_pp ~json5 v =
  Format.asprintf "@[<hv2>%a@]" (to_string_pp ~json5) v

let to_string ?(compact = true) ?(json5 = false) v =
  if compact then to_string_compact ~json5 v else to_string_pp ~json5 v
