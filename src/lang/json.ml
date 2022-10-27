type t = Json_base.t

(** A position. *)
type pos = Lexing.position * Lexing.position

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
    | _ -> Runtime_error.raise ~message:"Parse error" ~pos "json"

(* Special version of utf8 quoting that uses [Uchar.rep]
   when a character cannot be escaped. *)
let quote_utf8_string =
  let escape_char =
    let utf8_char_code s pos len =
      try Lang_string.utf8_char_code s pos len
      with _ -> Uchar.to_int Uchar.rep
    in
    Lang_string.escape_char ~escape_fun:(fun s pos len ->
        Printf.sprintf "\\u%04X" (utf8_char_code s pos len))
  in
  let next s i =
    try Lang_string.utf8_next s i with _ -> max (String.length s) (i + 1)
  in
  let escape_utf8_formatter =
    Lang_string.escape
      ~special_char:(fun s pos len ->
        if s.[pos] = '\'' && len = 1 then false
        else Lang_string.utf8_special_char s pos len)
      ~escape_char ~next
  in
  fun s ->
    Printf.sprintf "\"%s\"" (Lang_string.escape_string escape_utf8_formatter s)

let rec to_string_compact ~json5 = function
  | `Null -> "null"
  | `String s -> quote_utf8_string s
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Float f -> (
      match classify_float f with
        | FP_infinite when json5 -> if f < 0. then "-Infinity" else "Infinity"
        | FP_nan when json5 -> if f < 0. then "-NaN" else "NaN"
        | FP_infinite | FP_nan ->
            Runtime_error.raise ~pos:[]
              ~message:
                "Infinite or Nan number cannot be represented in JSON. You \
                 might want to consider using the `json5` representation."
              "json"
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
            (Lang_string.quote_string k)
            (to_string_pp ~json5) v
        in
        Format.fprintf f "{@;<1 0>%a@;<1 -2>}" (pp_list "," format_field) l
    | `Null | `String _ | `Int _ | `Float _ | `Bool _ ->
        Format.fprintf f "%s" (to_string_compact ~json5 v)

let to_string_pp ~json5 v =
  Format.asprintf "@[<hv2>%a@]" (to_string_pp ~json5) v

let to_string ?(compact = true) ?(json5 = false) v =
  if compact then to_string_compact ~json5 v else to_string_pp ~json5 v
