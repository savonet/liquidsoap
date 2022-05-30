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

let utf8_special_char s ofs len = len = 1 && ascii_special_char s ofs len
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

let escape_utf8_char =
  let utf8_char_code s pos len =
    try utf8_char_code s pos len with _ -> Uchar.to_int Uchar.rep
  in
  escape_char ~escape_fun:(fun s pos len ->
      Printf.sprintf "\\u%04X" (utf8_char_code s pos len))

let escape_utf8_formatter ?(special_char = utf8_special_char) =
  escape ~special_char ~escape_char:escape_utf8_char ~next:utf8_next

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

let escape_utf8_string ?special_char =
  escape_string (escape_utf8_formatter ?special_char)

let escape_ascii_string ?special_char =
  escape_string (escape_ascii_formatter ?special_char)

let quote_utf8_string s =
  Printf.sprintf "\"%s\""
    (escape_utf8_string
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

let quote_string s = try quote_utf8_string s with _ -> quote_ascii_string s
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
  let rex = Pcre.regexp_or unescape_patterns in
  Pcre.substitute ~rex ~subst:unescape_char

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

(** Remove line breaks from markdown text. This is useful for reflowing markdown such as when printing doc. *)
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
  let pager = if Sys.getenv_opt "PAGER" = Some "none" then false else pager in
  let default = output_string stdout in
  let cmd =
    let cmds = [("less", "-F -X"); ("more", "")] in
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
  type t = int list * string

  (* We assume something like, 2.0.0+git@7e211ffd *)
  let of_string s : t =
    let rex = Pcre.regexp "([\\.\\d]+)([^\\.]+)?" in
    let sub = Pcre.exec ~rex s in
    let num = Pcre.get_substring sub 1 in
    let str = try Pcre.get_substring sub 2 with Not_found -> "" in
    let num = String.split_on_char '.' num |> List.map int_of_string in
    (num, str)

  (** Number part. *)
  let num (v : t) = fst v

  (** String part. *)
  let str (v : t) = snd v

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
              let home = (Unix.getpwnam user).Unix.pw_dir in
              Filename.concat home (String.sub s index (len - index))
            with Not_found -> s)
        | _ -> s)
  in
  unrel
