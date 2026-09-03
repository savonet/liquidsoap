(* See yaml.mli for the supported subset of YAML. *)

type t =
  | Null
  | Bool of bool
  | Float of float
  | String of string
  | List of t list
  | Assoc of (string * t) list

exception Parse_error of string

let error lnum fmt =
  Printf.ksprintf
    (fun s -> raise (Parse_error (Printf.sprintf "line %d: %s" lnum s)))
    fmt

(* {1 String helpers} *)

let is_blank c = c = ' ' || c = '\t'

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && is_blank s.[!n - 1] do
    decr n
  done;
  String.sub s 0 !n

let skip_blanks s i =
  let n = String.length s in
  while !i < n && is_blank s.[!i] do
    incr i
  done

let add_utf8 b cp =
  if cp < 0x80 then Buffer.add_char b (Char.chr cp)
  else if cp < 0x800 then (
    Buffer.add_char b (Char.chr (0xc0 lor (cp lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
  else if cp < 0x10000 then (
    Buffer.add_char b (Char.chr (0xe0 lor (cp lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))
  else (
    Buffer.add_char b (Char.chr (0xf0 lor (cp lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3f)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3f))))

(* {1 Scalars} *)

(** Read [count] hexadecimal digits of [s] starting at [!i]. *)
let read_hex lnum s i count =
  let n = String.length s in
  let v = ref 0 in
  for _ = 1 to count do
    if !i >= n then error lnum "truncated unicode escape sequence";
    let d =
      match s.[!i] with
        | '0' .. '9' as c -> Char.code c - Char.code '0'
        | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
        | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
        | c -> error lnum "invalid hexadecimal digit %C" c
    in
    v := (!v * 16) + d;
    incr i
  done;
  !v

(** Read the double-quoted scalar of [s] starting at [!i]. *)
let read_double lnum s i =
  let n = String.length s in
  let b = Buffer.create 16 in
  let stop = ref false in
  incr i;
  while not !stop do
    if !i >= n then error lnum "unterminated double-quoted string";
    match s.[!i] with
      | '"' ->
          incr i;
          stop := true
      | '\\' -> (
          incr i;
          if !i >= n then error lnum "unterminated escape sequence";
          let e = s.[!i] in
          incr i;
          match e with
            | 'n' -> Buffer.add_char b '\n'
            | 't' -> Buffer.add_char b '\t'
            | 'r' -> Buffer.add_char b '\r'
            | 'b' -> Buffer.add_char b '\b'
            | 'f' -> Buffer.add_char b '\012'
            | 'v' -> Buffer.add_char b '\011'
            | 'a' -> Buffer.add_char b '\007'
            | 'e' -> Buffer.add_char b '\027'
            | '0' -> Buffer.add_char b '\000'
            | '\\' -> Buffer.add_char b '\\'
            | '"' -> Buffer.add_char b '"'
            | '/' -> Buffer.add_char b '/'
            | ' ' -> Buffer.add_char b ' '
            | 'x' | 'u' | 'U' ->
                let count = match e with 'x' -> 2 | 'u' -> 4 | _ -> 8 in
                let cp = read_hex lnum s i count in
                if cp > 0x10ffff then error lnum "invalid unicode code point";
                add_utf8 b cp
            | c -> error lnum "invalid escape sequence \\%c" c)
      | c ->
          Buffer.add_char b c;
          incr i
  done;
  Buffer.contents b

(** Read the single-quoted scalar of [s] starting at [!i]. *)
let read_single lnum s i =
  let n = String.length s in
  let b = Buffer.create 16 in
  let stop = ref false in
  incr i;
  while not !stop do
    if !i >= n then error lnum "unterminated single-quoted string";
    if s.[!i] = '\'' then
      if !i + 1 < n && s.[!i + 1] = '\'' then (
        Buffer.add_char b '\'';
        i := !i + 2)
      else (
        incr i;
        stop := true)
    else (
      Buffer.add_char b s.[!i];
      incr i)
  done;
  Buffer.contents b

(** Whether [s] is a decimal number, as OCaml's [float_of_string] is more
    permissive than YAML (it accepts ["0x1p3"], ["1_0"], ["nan"], ...). *)
let is_number s =
  let n = String.length s in
  let i = ref 0 in
  if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
  let start = !i in
  while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
    incr i
  done;
  let int_digits = !i - start in
  let frac_digits = ref 0 in
  if !i < n && s.[!i] = '.' then (
    incr i;
    let start = !i in
    while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
      incr i
    done;
    frac_digits := !i - start);
  if int_digits = 0 && !frac_digits = 0 then false
  else (
    let ok = ref true in
    if !i < n && (s.[!i] = 'e' || s.[!i] = 'E') then (
      incr i;
      if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
      let start = !i in
      while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
        incr i
      done;
      if !i = start then ok := false);
    !ok && !i = n)

(** Type of a plain (unquoted) scalar. *)
let resolve s =
  match s with
    | "" | "~" | "null" | "Null" | "NULL" -> Null
    | "true" | "True" | "TRUE" -> Bool true
    | "false" | "False" | "FALSE" -> Bool false
    | ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" -> Float infinity
    | "-.inf" | "-.Inf" | "-.INF" -> Float neg_infinity
    | ".nan" | ".NaN" | ".NAN" -> Float nan
    | _ -> if is_number s then Float (float_of_string s) else String s

(** Reject the YAML features we do not implement. *)
let check_unsupported lnum s =
  if s <> "" then (
    match s.[0] with
      | '|' | '>' -> error lnum "block scalars are not supported"
      | '&' -> error lnum "anchors are not supported"
      | '*' -> error lnum "aliases are not supported"
      | '!' -> error lnum "tags are not supported"
      | '%' -> error lnum "directives are not supported"
      | '@' | '`' -> error lnum "%C is a reserved indicator" s.[0]
      | '?' when String.length s = 1 || s.[1] = ' ' ->
          error lnum "complex mapping keys are not supported"
      | _ -> ())

(* {1 Flow collections} *)

(** Read a plain scalar of [s] starting at [!i], in flow context. *)
let read_plain s i =
  let n = String.length s in
  let start = !i in
  let fin = ref (-1) in
  while !fin < 0 && !i < n do
    let c = s.[!i] in
    if c = ',' || c = ']' || c = '}' then fin := !i
    else if
      c = ':'
      && (!i + 1 >= n
         || match s.[!i + 1] with ' ' | ',' | ']' | '}' -> true | _ -> false)
    then fin := !i
    else incr i
  done;
  let fin = if !fin < 0 then n else !fin in
  i := fin;
  rtrim (String.sub s start (fin - start))

let rec flow_value lnum s i =
  skip_blanks s i;
  if !i >= String.length s then error lnum "unexpected end of flow collection";
  match s.[!i] with
    | '[' ->
        incr i;
        flow_seq lnum s i
    | '{' ->
        incr i;
        flow_map lnum s i
    | '"' -> String (read_double lnum s i)
    | '\'' -> String (read_single lnum s i)
    | c ->
        let raw = read_plain s i in
        if raw = "" then error lnum "unexpected %C in flow collection" c;
        check_unsupported lnum raw;
        resolve raw

and flow_seq lnum s i =
  let n = String.length s in
  let items = ref [] in
  let stop = ref false in
  skip_blanks s i;
  if !i < n && s.[!i] = ']' then (
    incr i;
    stop := true);
  while not !stop do
    items := flow_value lnum s i :: !items;
    skip_blanks s i;
    if !i >= n then error lnum "unterminated flow sequence";
    match s.[!i] with
      | ']' ->
          incr i;
          stop := true
      | ',' ->
          incr i;
          skip_blanks s i;
          if !i < n && s.[!i] = ']' then (
            incr i;
            stop := true)
      | c -> error lnum "unexpected %C in flow sequence" c
  done;
  List (List.rev !items)

and flow_map lnum s i =
  let n = String.length s in
  let items = ref [] in
  let stop = ref false in
  skip_blanks s i;
  if !i < n && s.[!i] = '}' then (
    incr i;
    stop := true);
  while not !stop do
    let key = flow_key lnum s i in
    skip_blanks s i;
    let value =
      if !i < n && s.[!i] = ':' then (
        incr i;
        skip_blanks s i;
        if !i >= n then error lnum "unterminated flow mapping"
        else if s.[!i] = ',' || s.[!i] = '}' then Null
        else flow_value lnum s i)
      else Null
    in
    items := (key, value) :: !items;
    skip_blanks s i;
    if !i >= n then error lnum "unterminated flow mapping";
    match s.[!i] with
      | '}' ->
          incr i;
          stop := true
      | ',' ->
          incr i;
          skip_blanks s i;
          if !i < n && s.[!i] = '}' then (
            incr i;
            stop := true)
      | c -> error lnum "unexpected %C in flow mapping" c
  done;
  Assoc (List.rev !items)

and flow_key lnum s i =
  skip_blanks s i;
  if !i >= String.length s then error lnum "unterminated flow mapping";
  match s.[!i] with
    | '"' -> read_double lnum s i
    | '\'' -> read_single lnum s i
    | '[' | '{' -> error lnum "only scalar mapping keys are supported"
    | _ ->
        let raw = read_plain s i in
        if raw = "" then error lnum "empty mapping key";
        check_unsupported lnum raw;
        raw

(* {1 Lines} *)

type line = { mutable indent : int; mutable content : string; lnum : int }

(** Remove a trailing comment, taking quoting into account. *)
let strip_comment s =
  let n = String.length s in
  let i = ref 0 in
  let cut = ref (-1) in
  let quote = ref ' ' in
  while !cut < 0 && !i < n do
    let c = s.[!i] in
    if !quote = '"' then
      if c = '\\' then i := !i + 2
      else (
        if c = '"' then quote := ' ';
        incr i)
    else if !quote = '\'' then (
      if c = '\'' then quote := ' ';
      incr i)
    else if c = '#' && (!i = 0 || is_blank s.[!i - 1]) then cut := !i
    else (
      if c = '"' || c = '\'' then quote := c;
      incr i)
  done;
  if !cut < 0 then s else String.sub s 0 !cut

(** Split the input into non-empty, comment-free lines. *)
let scan src =
  let lines = ref [] in
  List.iteri
    (fun k l ->
      let lnum = k + 1 in
      let l =
        let n = String.length l in
        if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l
      in
      let l = strip_comment l in
      let n = String.length l in
      let i = ref 0 in
      while !i < n && l.[!i] = ' ' do
        incr i
      done;
      if !i < n && l.[!i] = '\t' then
        error lnum "tabulations cannot be used for indentation";
      let content = rtrim (String.sub l !i (n - !i)) in
      if content <> "" then lines := { indent = !i; content; lnum } :: !lines)
    (String.split_on_char '\n' src);
  Array.of_list (List.rev !lines)

(** Handle the [---] and [...] document markers. *)
let strip_document_markers lines =
  let is_marker m l = l.indent = 0 && l.content = m in
  let lines = Array.to_list lines in
  let lines =
    match lines with l :: rest when is_marker "---" l -> rest | _ -> lines
  in
  let rec take acc = function
    | [] -> List.rev acc
    | l :: rest when is_marker "..." l -> (
        match rest with
          | [] -> List.rev acc
          | l :: _ -> error l.lnum "content after the end of the document")
    | l :: _ when is_marker "---" l ->
        error l.lnum "multiple documents are not supported"
    | l :: rest -> take (l :: acc) rest
  in
  Array.of_list (take [] lines)

(* {1 Block structure} *)

let is_dash c = c = "-" || (String.length c >= 2 && c.[0] = '-' && c.[1] = ' ')

(** Position of the [:] separating a key from its value, if any. *)
let key_sep s =
  let n = String.length s in
  let res = ref None in
  let i = ref 0 in
  let depth = ref 0 in
  let quote = ref ' ' in
  while !res = None && !i < n do
    let c = s.[!i] in
    if !quote = '"' then
      if c = '\\' then i := !i + 2
      else (
        if c = '"' then quote := ' ';
        incr i)
    else if !quote = '\'' then (
      if c = '\'' then quote := ' ';
      incr i)
    else (
      (match c with
        | '"' | '\'' -> quote := c
        | '[' | '{' -> incr depth
        | ']' | '}' -> decr depth
        | ':' when !depth = 0 && (!i + 1 >= n || s.[!i + 1] = ' ') ->
            res := Some !i
        | _ -> ());
      incr i)
  done;
  !res

let parse_key lnum k =
  let k = String.trim k in
  if k = "" then error lnum "empty mapping key";
  check_unsupported lnum k;
  let consumed v i =
    skip_blanks k i;
    if !i <> String.length k then error lnum "unexpected content after key";
    v
  in
  match k.[0] with
    | '"' ->
        let i = ref 0 in
        consumed (read_double lnum k i) i
    | '\'' ->
        let i = ref 0 in
        consumed (read_single lnum k i) i
    | '[' | '{' -> error lnum "only scalar mapping keys are supported"
    | _ -> k

(** Parse a value written on a single line. *)
let parse_inline lnum s =
  let s = String.trim s in
  check_unsupported lnum s;
  let consumed v i =
    skip_blanks s i;
    if !i <> String.length s then error lnum "unexpected content after value";
    v
  in
  if s = "" then Null
  else (
    match s.[0] with
      | '[' | '{' ->
          let i = ref 0 in
          consumed (flow_value lnum s i) i
      | '"' ->
          let i = ref 0 in
          consumed (String (read_double lnum s i)) i
      | '\'' ->
          let i = ref 0 in
          consumed (String (read_single lnum s i)) i
      | _ -> resolve s)

let parse lines =
  let len = Array.length lines in
  let pos = ref 0 in
  (* Parse the block of lines at indentation [indent]. *)
  let rec node indent =
    if !pos >= len then Null
    else (
      let l = lines.(!pos) in
      if l.indent < indent then Null
      else if l.indent > indent then error l.lnum "unexpected indentation"
      else if is_dash l.content then seq indent
      else (
        match key_sep l.content with
          | Some _ -> map indent
          | None ->
              incr pos;
              parse_inline l.lnum l.content))
  (* Parse the block nested under a line at indentation [indent]. *)
  and child indent =
    if !pos < len && lines.(!pos).indent > indent then node lines.(!pos).indent
    else Null
  and seq indent =
    let items = ref [] in
    while
      !pos < len && lines.(!pos).indent = indent && is_dash lines.(!pos).content
    do
      let l = lines.(!pos) in
      let c = l.content in
      let n = String.length c in
      let i = ref 1 in
      while !i < n && c.[!i] = ' ' do
        incr i
      done;
      let v =
        if !i >= n then (
          incr pos;
          child indent)
        else (
          (* Reinterpret what follows the dash as a line of its own, which
             handles ["- 5"], ["- a: 1"] with further entries below, and
             ["- - 1"] uniformly. *)
          l.indent <- indent + !i;
          l.content <- String.sub c !i (n - !i);
          node l.indent)
      in
      items := v :: !items
    done;
    List (List.rev !items)
  and map indent =
    let items = ref [] in
    while !pos < len && lines.(!pos).indent = indent do
      let l = lines.(!pos) in
      if is_dash l.content then
        error l.lnum "unexpected sequence entry in a mapping";
      match key_sep l.content with
        | None -> error l.lnum "expected a mapping entry"
        | Some k ->
            let key = parse_key l.lnum (String.sub l.content 0 k) in
            let n = String.length l.content in
            let rest = String.trim (String.sub l.content (k + 1) (n - k - 1)) in
            incr pos;
            let v =
              if rest <> "" then parse_inline l.lnum rest
              else if
                (* A sequence may be written at the indentation of its key. *)
                !pos < len
                && lines.(!pos).indent = indent
                && is_dash lines.(!pos).content
              then seq indent
              else child indent
            in
            items := (key, v) :: !items
    done;
    Assoc (List.rev !items)
  in
  let v = if len = 0 then Null else node lines.(0).indent in
  if !pos < len then error lines.(!pos).lnum "unexpected content";
  v

let of_string s : (t, string) result =
  try Ok (parse (strip_document_markers (scan s)))
  with Parse_error msg -> Error msg

let of_file f =
  let ic = open_in_bin f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  of_string s

(* {1 Printing} *)

let float_to_string f =
  if Float.is_nan f then ".nan"
  else if f = Float.infinity then ".inf"
  else if f = Float.neg_infinity then "-.inf"
  else if Float.is_integer f && Float.abs f < 1e16 then Printf.sprintf "%.0f" f
  else (
    let s = Printf.sprintf "%.15g" f in
    if float_of_string s = f then s else Printf.sprintf "%.17g" f)

(** Whether [s] can be printed without quotes and read back identically. *)
let plain_safe s =
  let n = String.length s in
  n > 0
  && (match resolve s with String _ -> true | _ -> false)
  && s <> "---" && s <> "..."
  && (not (is_blank s.[0]))
  && (not (is_blank s.[n - 1]))
  && (match s.[0] with
    | ',' | '[' | ']' | '{' | '}' | '#' | '&' | '*' | '!' | '|' | '>' | '\''
    | '"' | '%' | '@' | '`' ->
        false
    | '-' | '?' | ':' -> n > 1 && s.[1] <> ' '
    | _ -> true)
  &&
  let ok = ref true in
  String.iteri
    (fun i c ->
      if c < ' ' || c = '\127' then ok := false
      else if c = ':' && (i + 1 >= n || s.[i + 1] = ' ') then ok := false
      else if c = '#' && i > 0 && is_blank s.[i - 1] then ok := false)
    s;
  !ok

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '\n' -> Buffer.add_string b "\\n"
        | '\t' -> Buffer.add_string b "\\t"
        | '\r' -> Buffer.add_string b "\\r"
        | c when c < ' ' || c = '\127' ->
            Buffer.add_string b (Printf.sprintf "\\x%02x" (Char.code c))
        | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let render_string s = if plain_safe s then s else quote s

let render_scalar v =
  match v with
    | Null -> "null"
    | Bool b -> if b then "true" else "false"
    | Float f -> float_to_string f
    | String s -> render_string s
    | List [] -> "[]"
    | Assoc [] -> "{}"
    | List _ | Assoc _ -> assert false

(** Print [v] at indentation [indent], assuming the prefix of its first line is
    already printed. Always ends with a newline. *)
let rec write b indent v =
  match v with
    | List (_ :: _ as items) ->
        List.iteri
          (fun i x ->
            if i > 0 then Buffer.add_string b (String.make indent ' ');
            Buffer.add_string b "- ";
            write b (indent + 2) x)
          items
    | Assoc (_ :: _ as items) ->
        List.iteri
          (fun i (k, x) ->
            if i > 0 then Buffer.add_string b (String.make indent ' ');
            Buffer.add_string b (render_string k);
            Buffer.add_char b ':';
            match x with
              | List (_ :: _) | Assoc (_ :: _) ->
                  Buffer.add_char b '\n';
                  Buffer.add_string b (String.make (indent + 2) ' ');
                  write b (indent + 2) x
              | _ ->
                  Buffer.add_char b ' ';
                  write b (indent + 2) x)
          items
    | v ->
        Buffer.add_string b (render_scalar v);
        Buffer.add_char b '\n'

let to_string yaml =
  let b = Buffer.create 256 in
  write b 0 yaml;
  Buffer.contents b

let to_file f yaml =
  let oc = open_out_bin f in
  output_string oc (to_string yaml);
  close_out oc
