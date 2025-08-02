module String = Stdlib.String

(* Generic escaping function. Returns a list of escaping substitutions. *)
let escape ~special_char ~next ~escape_char s =
  let orig_len = String.length s in
  let rec f (segments, total_len) pos =
    let next_pos = next s (pos : int) in
    let len = next_pos - pos in
    let ret =
      if special_char s pos len then (
        let s = escape_char s pos len in
        let len = String.length s in
        (`Subst (s, len) :: segments, total_len + len))
      else
        ( (match segments with
            | `Orig (pos, s_len) :: segments ->
                `Orig (pos, s_len + len) :: segments
            | segments -> `Orig (pos, len) :: segments),
          total_len + len )
    in
    if next_pos < orig_len then f ret next_pos else ret
  in
  if orig_len > 0 then f ([], 0) 0 else ([], 0)

let utf8_next s i =
  match s.[i] with
    | '\000' .. '\127' -> i + 1
    | '\192' .. '\223' -> i + 2
    | '\224' .. '\239' -> i + 3
    | '\240' .. '\247' -> i + 4
    | _ -> failwith "invalid utf8"

(* Return the utf8 char code of the first utf8 character in the given
   string. *)
let utf8_char_code s pos len =
  if len < 1 then failwith "invalid utf8";
  match s.[pos] with
    | '\000' .. '\127' as c -> Char.code c
    | '\192' .. '\223' as c ->
        if len < 2 then failwith "invalid utf8";
        let n1 = Char.code c in
        let n2 = Char.code s.[pos + 1] in
        if n2 lsr 6 != 0b10 then failwith "invalid utf8";
        ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)
    | '\224' .. '\239' as c ->
        if len < 3 then failwith "invalid utf8";
        let n1 = Char.code c in
        let n2 = Char.code s.[pos + 1] in
        let n3 = Char.code s.[pos + 2] in
        if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 then failwith "invalid utf8";
        let p =
          ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
        in
        if p >= 0xd800 && p <= 0xdf00 then failwith "invalid utf8";
        p
    | '\240' .. '\247' as c ->
        if len < 4 then failwith "invalid utf8";
        let n1 = Char.code c in
        let n2 = Char.code s.[pos + 1] in
        let n3 = Char.code s.[pos + 2] in
        let n4 = Char.code s.[pos + 3] in
        if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 || n4 lsr 6 != 0b10 then
          failwith "invalid utf8";
        ((n1 land 0x07) lsl 18)
        lor ((n2 land 0x3f) lsl 12)
        lor ((n3 land 0x3f) lsl 6)
        lor (n4 land 0x3f)
    | _ -> failwith "invalid utf8"

(* End of Extlib code *)

let is_valid_utf8_code_point s pos len =
  try
    ignore (utf8_char_code s pos len);
    true
  with _ -> false

let ascii_special_char s pos len =
  match (s.[pos], len) with
    | '\'', 1
    | '"', 1
    | '\\', 1
    (* DEL *)
    | '\x7F', 1 ->
        true
    (* Control chars *)
    | c, 1 when Char.code c <= 0x1F -> true
    | c, 1 when Char.code c > 0x7E -> true
    | _ -> false

let utf8_special_char s ofs len =
  (not (is_valid_utf8_code_point s ofs len))
  || (len = 1 && ascii_special_char s ofs len)

let ascii_next _ i = i + 1

let escape_char ~escape_fun s pos len =
  match (s.[pos], len) with
    | '\b', 1 -> "\\b"
    | '\n', 1 -> "\\n"
    | '\r', 1 -> "\\r"
    | '\t', 1 -> "\\t"
    | '\\', 1 -> "\\\\"
    | '"', 1 -> "\\\""
    | '\'', 1 -> "\\'"
    | _ -> escape_fun s pos len

let escape_utf8_char ~strict =
  let utf8_char_code s pos len =
    try utf8_char_code s pos len
    with _ when not strict -> Uchar.to_int Uchar.rep
  in
  escape_char ~escape_fun:(fun s pos len ->
      Printf.sprintf "\\u%04X" (utf8_char_code s pos len))

let escape_utf8_formatter ?(strict = false) ?(special_char = utf8_special_char)
    =
  escape ~special_char ~escape_char:(escape_utf8_char ~strict) ~next:utf8_next

let escape_hex_char =
  escape_char ~escape_fun:(fun s pos len ->
      assert (len = 1);
      Printf.sprintf "\\x%02X" (int_of_char s.[pos]))

let escape_octal_char =
  escape_char ~escape_fun:(fun s pos len ->
      assert (len = 1);
      Printf.sprintf "\\%03o" (int_of_char s.[pos]))

(* We use the \xhh syntax to make sure the resulting string is also consistently readable in
   OCaml. \nnn can be tricky since they are octal sequences in liquidsoap and digital sequences
   in OCaml. *)
let escape_ascii_formatter ?(special_char = ascii_special_char) =
  escape ~special_char ~escape_char:escape_hex_char ~next:ascii_next

let has_subst = List.exists (function `Subst _ -> true | _ -> false)

let escape_string escape s =
  let segments, len = escape s in
  if not (has_subst segments) then s
  else (
    let b = Bytes.create len in
    ignore
      (List.fold_left
         (fun b_pos segment ->
           match segment with
             | `Orig (s_pos, len) ->
                 let b_pos = b_pos - len in
                 Bytes.blit_string s s_pos b b_pos len;
                 b_pos
             | `Subst (subst, len) ->
                 let b_pos = b_pos - len in
                 Bytes.blit_string subst 0 b b_pos len;
                 b_pos)
         len segments);
    Bytes.unsafe_to_string b)

let escape_utf8_string ?strict ?special_char =
  escape_string (escape_utf8_formatter ?strict ?special_char)

let escape_ascii_string ?special_char =
  escape_string (escape_ascii_formatter ?special_char)

let quote_utf8_string ?strict s =
  Printf.sprintf "\"%s\""
    (escape_utf8_string ?strict
       ~special_char:(fun s pos len ->
         if s.[pos] = '\'' && len = 1 then false
         else utf8_special_char s pos len)
       s)

let quote_ascii_string s =
  Printf.sprintf "\"%s\""
    (escape_ascii_string
       ~special_char:(fun s pos len ->
         if s.[pos] = '\'' && len = 1 then false
         else ascii_special_char s pos len)
       s)

let quote_string s =
  try quote_utf8_string ~strict:true s with _ -> quote_ascii_string s

let unescape_utf8_pattern = "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]"
let unescape_hex_pattern = "\\\\x[0-9a-fA-F][0-9a-fA-F]"
let unescape_octal_pattern = "\\\\[0-9][0-9][0-9]"

let unescape_patterns =
  [
    "\\\\a";
    "\\\\b";
    "\\\\e";
    "\\\\f";
    "\\\\n";
    "\\\\r";
    "\\\\t";
    "\\\\v";
    "\\\\/";
    "\\\\\\\\";
    "\\\\\"";
    "\\\\\'";
    "\\\\\\?";
    unescape_octal_pattern;
    unescape_hex_pattern;
    unescape_utf8_pattern;
  ]

let unescape_octal_char s =
  let s = String.sub s 1 3 in
  Printf.sprintf "%c" (Char.chr (int_of_string ("0o" ^ s)))

let unescape_hex_char s =
  let s = String.sub s 2 2 in
  Printf.sprintf "%c" (Char.chr (int_of_string ("0x" ^ s)))

let unescape_utf8_char s =
  let b = Buffer.create 1 in
  let s = String.sub s 2 4 in
  let c = try Uchar.of_int (int_of_string ("0x" ^ s)) with _ -> Uchar.rep in
  Buffer.add_utf_8_uchar b c;
  Buffer.contents b

let unescape_char = function
  | "\\a" -> "\x07"
  | "\\b" -> "\b"
  | "\\e" -> "\x1b"
  | "\\f" -> "\x0c"
  | "\\n" -> "\n"
  | "\\r" -> "\r"
  | "\\t" -> "\t"
  | "\\v" -> "\x0b"
  | "\\/" -> "/"
  | "\\\\" -> "\\"
  | "\\\"" -> "\""
  | "\\'" -> "'"
  | "\\?" -> "\x3f"
  | s when String.length s = 6 -> unescape_utf8_char s
  | s when String.length s = 4 && s.[1] = 'x' -> unescape_hex_char s
  | s when String.length s = 4 -> unescape_octal_char s
  | _ -> assert false

let unescape_string =
  Re.replace ~all:true
    ~f:(fun g -> unescape_char (Re.Group.get g 0))
    (Re.Pcre.regexp (String.concat "|" unescape_patterns))
