let getpwnam = ref (fun _ -> raise Not_found)

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

(** String representation of a matrix of strings. *)
let string_of_matrix a =
  let height = Array.length a in
  let width = Array.fold_left (fun h a -> max h (Array.length a)) 0 a in
  let len = Array.make width 0 in
  for j = 0 to height - 1 do
    for i = 0 to Array.length a.(j) - 1 do
      len.(i) <- max len.(i) (String.length a.(j).(i))
    done
  done;
  let ans = Buffer.create 10 in
  for j = 0 to height - 1 do
    for i = 0 to Array.length a.(j) - 1 do
      let s = a.(j).(i) in
      if i <> 0 then Buffer.add_string ans " ";
      Buffer.add_string ans s;
      Buffer.add_string ans (String.make (len.(i) - String.length s) ' ')
    done;
    Buffer.add_string ans "\n"
  done;
  Buffer.contents ans

(** Remove line breaks from markdown text. This is useful for reflowing markdown
    such as when printing doc. *)
let unbreak_md md =
  let must_break = function
    | "" :: _ -> true
    | "```" :: _ -> true
    | line :: _ when line.[0] = '-' (* itemize *) -> true
    | _ -> false
  in
  let rec text = function
    | [] -> ""
    | [line] -> line
    | "```" :: lines -> "```\n" ^ verb lines
    | line :: lines when line = "" || must_break lines ->
        line ^ "\n" ^ text lines
    | line :: lines -> line ^ " " ^ text lines
  and verb = function
    | "```" :: lines -> "```\n" ^ text lines
    | line :: lines -> line ^ "\n" ^ verb lines
    | [] -> "```"
  in
  text (String.split_on_char '\n' md)

(* From dbuenzli/cmdliner *)
let find_cmd cmds =
  let test, null =
    match Sys.os_type with
      | "Win32" -> ("where", " NUL")
      | _ -> ("type", "/dev/null")
  in
  let rec cmd = function
    | (c, args) :: l ->
        if Sys.command (Printf.sprintf "%s %s 1>%s 2>%s" test c null null) = 0
        then Some (if args = "" then c else c ^ " " ^ args)
        else cmd l
    | [] -> None
  in
  cmd cmds

let print_string ?(pager = false) s =
  let pager =
    if
      Sys.win32
      || Sys.getenv_opt "TERM" = None
      || Sys.getenv_opt "PAGER" = Some "none"
    then false
    else pager
  in
  let default = output_string stdout in
  let cmd =
    let cmds = [("less", "-F -X -r -f"); ("more", "")] in
    let cmds = try (Sys.getenv "PAGER", "") :: cmds with Not_found -> cmds in
    let cmds =
      try (Sys.getenv "MANPAGER", "") :: cmds with Not_found -> cmds
    in
    find_cmd cmds
  in
  match (pager, cmd) with
    | false, _ | _, None -> default s
    | true, Some pager -> (
        let fname, oc = Filename.open_temp_file "liquidsoap" ".txt" in
        try
          ignore (Sys.command "clear");
          output_string oc s;
          flush oc;
          if Sys.command (Printf.sprintf "%s %s" pager fname) <> 0 then
            default s;
          raise Exit
        with _ ->
          close_out oc;
          Unix.unlink fname)

let kprint_string ?(pager = false) f =
  if not pager then f (print_string ~pager)
  else (
    let ans = Buffer.create 10 in
    f (Buffer.add_string ans);
    print_string ~pager (Buffer.contents ans))

(** Operations on versions of Liquidsoap. *)
module Version = struct
  open Term_hash

  type t = int list * string [@@deriving hash]

  (* We assume something like, 2.0.0+git@7e211ffd *)
  let of_string s : t =
    let rex = Re.Pcre.regexp "(?:rolling-release-v)?([\\.\\dx]+)([^\\.]+)?" in
    let sub = Re.Pcre.exec ~rex s in
    let num = Re.Pcre.get_substring sub 1 in
    let str = try Re.Pcre.get_substring sub 2 with _ -> "" in
    let num =
      let int_of_string s = if s = "x" then -1 else int_of_string s in
      String.split_on_char '.' num |> List.map int_of_string
    in
    (num, str)

  (** Number part. *)
  let num (v : t) = fst v

  (** String part. *)
  let str (v : t) = snd v

  let to_string v =
    Printf.sprintf "%s%s"
      (String.concat "." (List.map string_of_int (num v)))
      (str v)

  (** Compare two versions. 2.0.0~foo is less than 2.0.0 *)
  let compare v w =
    let compare_suffixes =
      match (str v, str w) with
        | v, w
          when String.length v > 1
               && String.length w > 1
               && v.[0] = '~'
               && w.[0] = '~' ->
            String.compare v w
        | v, w
          when String.length v > 1
               && String.length w > 1
               && v.[0] = '~'
               && w.[0] <> '~' ->
            -1
        | v, w
          when String.length v > 1
               && String.length w > 1
               && v.[0] <> '~'
               && w.[0] = '~' ->
            1
        | v, "" when String.length v > 1 && v.[0] = '~' -> -1
        | "", w when String.length w > 1 && w.[0] = '~' -> 1
        | v, w -> String.compare v w
    in
    let rec aux v w =
      match (v, w) with
        | m :: _, n :: _ when m < n -> -1
        | m :: _, n :: _ when m > n -> 1
        | _ :: v, _ :: w -> aux v w
        | [], [] -> compare_suffixes
        | [], _ -> -1
        | _, [] -> 1
    in
    aux (num v) (num w)
end

(** Expand ~ notation in filenames. *)
let home_unrelate =
  let home = Sys.getenv_opt "HOME" in
  let unrel s =
    let len = String.length s in
    if len < 2 then (match home with Some h when s = "~" -> h | _ -> s)
    else (
      match (home, s.[0] = '~', s.[1] = '/') with
        | Some home, true, true ->
            (* Something like ~/data/file *)
            Filename.concat home (String.sub s 2 (len - 2))
        | _, true, false -> (
            (* Something like ~bob/data/file *)
            let index = try String.index s '/' with Not_found -> len in
            let user = String.sub s 1 (index - 1) in
            try
              let home = (!getpwnam user).Unix.pw_dir in
              Filename.concat home (String.sub s index (len - index))
            with Not_found -> s)
        | _ -> s)
  in
  unrel

(** Generate an identifier from the name of the source. *)
let generate_id =
  let t = Hashtbl.create 10 in
  fun name ->
    if not (Hashtbl.mem t name) then Hashtbl.replace t name (ref 0);
    let n = Hashtbl.find t name in
    incr n;
    if !n = 1 then name else name ^ "." ^ string_of_int !n

(** Decode Base64-encoded data *)
let decode64 s =
  let padding = ref 0 in
  let to_int c =
    match c with
      | 'A' .. 'Z' -> int_of_char c - int_of_char 'A'
      | 'a' .. 'z' -> int_of_char c - int_of_char 'a' + 26
      | '0' .. '9' -> int_of_char c - int_of_char '0' + 52
      | '+' -> 62
      | '/' -> 63
      | '=' ->
          incr padding;
          0
      | _ -> failwith "decode64: invalid encoding"
  in
  let result = ref [] in
  let add x = result := char_of_int x :: !result in
  for i = 0 to (String.length s / 4) - 1 do
    (* Read 4 64-digits, i.e. 3 bytes. *)
    let c n = to_int s.[(i * 4) + n] in
    let i = c 3 + (c 2 lsl 6) + (c 1 lsl 12) + (c 0 lsl 18) in
    add ((i land 0xff0000) lsr 16);
    add ((i land 0x00ff00) lsr 8);
    add (i land 0x0000ff)
  done;
  let result =
    (* Remove up to two bytes depending on the padding. *)
    match !padding with
      | 0 -> !result
      | 1 -> List.tl !result
      | 2 -> List.tl (List.tl !result)
      | _ -> failwith "decode64: invalid encoding"
  in
  let len = List.length result in
  let s = Bytes.make len ' ' in
  ignore
    (List.fold_left
       (fun i c ->
         Bytes.set s i c;
         i - 1)
       (len - 1) result);
  Bytes.unsafe_to_string s

(** Base 64 encoding. *)
let encode64 s =
  let digit =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let extra = String.length s mod 3 in
  let s = match extra with 1 -> s ^ "\000\000" | 2 -> s ^ "\000" | _ -> s in
  let n = String.length s in
  let dst = Bytes.create (4 * (n / 3)) in
  for i = 0 to (n / 3) - 1 do
    let ( := ) j v = Bytes.set dst ((i * 4) + j) digit.[v] in
    let c j = int_of_char s.[(i * 3) + j] in
    let c0 = c 0 and c1 = c 1 and c2 = c 2 in
    0 := c0 lsr 2;
    1 := (c0 lsl 4) land 63 lor (c1 lsr 4);
    2 := (c1 lsl 2) land 63 lor (c2 lsr 6);
    3 := c2 land 63
  done;
  if extra = 1 then (
    Bytes.set dst ((4 * (n / 3)) - 2) '=';
    Bytes.set dst ((4 * (n / 3)) - 1) '=')
  else if extra = 2 then Bytes.set dst ((4 * (n / 3)) - 1) '=';
  Bytes.unsafe_to_string dst

(* URL encoding/decoding according to RFC 1738, RFC 1630.
 * Borrowed from ocamlnet. *)

(** Converts k to a 2-digit hexadecimal string. *)
let to_hex2 =
  let hex_digits =
    [|
      '0';
      '1';
      '2';
      '3';
      '4';
      '5';
      '6';
      '7';
      '8';
      '9';
      'A';
      'B';
      'C';
      'D';
      'E';
      'F';
    |]
  in
  fun k ->
    let s = Bytes.create 2 in
    Bytes.set s 0 hex_digits.((k lsr 4) land 15);
    Bytes.set s 1 hex_digits.(k land 15);
    Bytes.unsafe_to_string s

let url_encode ?(plus = true) s =
  Re.replace ~all:true
    ~f:(fun g ->
      let x = Re.Group.get g 0 in
      if plus && x = " " then "+"
      else (
        let k = Char.code x.[0] in
        "%" ^ to_hex2 k))
    (Re.Pcre.regexp "[^A-Za-z0-9_.!*-]")
    s

let of_hex1 c =
  match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | _ -> failwith "invalid url"

let url_decode ?(plus = true) s =
  let rex = Re.Pcre.regexp "\\+|%..|%.|%" in
  Re.replace ~all:true
    ~f:(fun g ->
      let s = Re.Group.get g 0 in
      if s = "+" then if plus then " " else "+"
      else (
        (* Assertion: s.[0] = '%' *)
        if String.length s < 3 then failwith "invalid url";
        let k1 = of_hex1 s.[1] in
        let k2 = of_hex1 s.[2] in
        String.make 1 (Char.chr ((k1 lsl 4) lor k2))))
    rex s
