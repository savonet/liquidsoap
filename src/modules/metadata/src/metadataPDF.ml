open MetadataBase
module R = Reader

let is_digit c = '0' <= c && c <= '9'

let is_octal_digit c = '0' <= c && c <= '7'

let is_space c = c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\012'

(* Decode Hex String: <48656C6C6F> -> "Hello" *)
let decode_hex_string content =
  let b = Buffer.create (String.length content / 2) in
  let rec loop i =
    if i >= String.length content then Buffer.contents b
    else
      let c = content.[i] in
      if is_space c then loop (i + 1)
      else if i + 1 < String.length content then
        let hex_pair = String.sub content i 2 in
        try
          let char_code = int_of_string ("0x" ^ hex_pair) in
          Buffer.add_char b (char_of_int char_code);
          loop (i + 2)
        with _ -> loop (i + 1) (* Skip invalid *)
      else loop (i + 1)
  in
  loop 0

(* Decode Literal String: (Hello\nWorld) -> "Hello\nWorld" *)
let decode_literal_string content =
  let b = Buffer.create @@ String.length content in
  let len = String.length content in
  let rec loop i =
    if i >= len then Buffer.contents b
    else
      match content.[i] with
      | '\\' ->
         (* Handle escaped character. *)
         if i + 1 >= len then (Buffer.add_char b '\\'; Buffer.contents b)
         else
           (match content.[i+1] with
            | 'n' -> Buffer.add_char b '\n'; loop (i + 2)
            | 'r' -> Buffer.add_char b '\r'; loop (i + 2)
            | 't' -> Buffer.add_char b '\t'; loop (i + 2)
            | 'b' -> Buffer.add_char b '\b'; loop (i + 2)
            | 'f' -> Buffer.add_char b '\012'; loop (i + 2)
            | '(' -> Buffer.add_char b '('; loop (i + 2)
            | ')' -> Buffer.add_char b ')'; loop (i + 2)
            | '\r' -> loop (i + 2)
            | '\\' -> Buffer.add_char b '\\'; loop (i + 2)
            | d when is_digit d -> (* Octal \ddd *)
               let end_oct = min (i + 4) len in
               let oct_str = String.sub content (i + 1) (end_oct - (i + 1)) in
               (* Take up to 3 digits *)
               let oct_len =
                 let rec count k =
                   if k < String.length oct_str && is_octal_digit oct_str.[k] then count (k+1)
                   else k
                 in
                 count 0
               in
               if oct_len > 0 then
                 let code = int_of_string ("0o" ^ String.sub oct_str 0 oct_len) in
                 Buffer.add_char b (char_of_int code);
                 loop (i + 1 + oct_len)
               else (Buffer.add_char b d; loop (i + 2))
            | c -> Buffer.add_char b c; loop (i + 2))
      | c -> Buffer.add_char b c; loop (i + 1)
  in
  loop 0

(* Parse the dictionary << /Key (Value) ... >> *)
let parse_metadata content =
  let len = String.length content in
  let ans = ref [] in

  let rec skip_whitespace i =
    if i >= len then i
    else if is_space content.[i] then skip_whitespace (i + 1)
    else i
  in

  let rec parse i =
    let i = skip_whitespace i in
    if i >= len then ()
    else if content.[i] = '/' then
      (* Found a key *)
      let key_start = i + 1 in
      let rec find_key_end k =
        if k >= len || is_space content.[k] || content.[k] = '(' || content.[k] = '<' || content.[k] = '/' || content.[k] = '>'
        then k
        else find_key_end (k + 1)
      in
      let key_end = find_key_end key_start in
      let key = String.sub content key_start (key_end - key_start) in

      let val_start = skip_whitespace key_end in
      if val_start >= len then ()
      else
        match content.[val_start] with
        | '(' ->
           (* Literal String *)
           let rec find_end k depth =
             if k >= len then k
             else match content.[k] with
                  | '\\' -> find_end (k + 2) depth (* Skip escaped char *)
                  | '(' -> find_end (k + 1) (depth + 1)
                  | ')' -> if depth = 0 then k else find_end (k + 1) (depth - 1)
                  | _ -> find_end (k + 1) depth
           in
           let val_end = find_end (val_start + 1) 0 in
           let raw_val = String.sub content (val_start + 1) (val_end - (val_start + 1)) in
           ans := (key, decode_literal_string raw_val) :: !ans;
           parse (val_end + 1)

        | '<' ->
           (* Hex String or Dictionary *)
           if val_start + 1 < len && content.[val_start+1] = '<' then
             (* Nested dict start, skip for now *)
             parse (val_start + 1)
           else
             let rec find_end k =
               if k >= len then k
               else if content.[k] = '>' then k
               else find_end (k + 1)
             in
             let val_end = find_end (val_start + 1) in
             let raw_val = String.sub content (val_start + 1) (val_end - (val_start + 1)) in
             ans := (key, decode_hex_string raw_val) :: !ans;
             parse (val_end + 1)
        | _ ->
           (* Other types (numbers, bools), skip to next slash *)
           let rec find_next_slash k =
             if k >= len || content.[k] = '/' || content.[k] = '>' then k
             else find_next_slash (k + 1)
           in
           parse (find_next_slash val_start)
    else
      (* Skip unknown chars usually associated with dictionary start/end delimiters *)
      parse (i + 1)
  in
  parse 0;
  !ans

let recode s =
  if String.starts_with ~prefix:"\xfe\xff" s then MetadataCharEncoding.Naive.convert ~source:`UTF_16 s
  else MetadataCharEncoding.Naive.convert ~source:`ISO_8859_1 s

(* Find a substring at the beginning of a line *)
let find f s =
  let is_newline c = c = '\r' || c = '\n' in
  let find_newline () = while not (is_newline @@ (R.read f 1).[0]) do () done in
  let n = String.length s in
  try
    find_newline ();
    while not (R.read f n = s) do
      f.seek (-(n-1));
      find_newline ()
    done;
    true
  with
  | _ -> false

let parse f : metadata =
  if R.read f 5 <> "%PDF-" then raise Invalid;
  (* Find a string of the form "/Info 123 0 R" to obtain object id (123 0). *)
  (* Note: it would be better to search from the end: new objects are added at the end when editing. *)
  let find_slash () = while R.read f 1 <> "/" do () done in
  find_slash ();
  while R.read f 4 <> "Info" do
    find_slash ()
  done;
  if R.read f 1 <> " " then raise Invalid;
  let read_int () =
    let n = ref "" in
    let s = ref @@ R.read f 1 in
    while !s <> " " do
      if not (is_digit !s.[0]) then raise Invalid;
      n := !n ^ !s;
      s := R.read f 1
    done;
    !n
  in
  let obj_id = read_int () in
  let gen_id = read_int () in
  (* Find the actual object, ie the contents of 123 0 obj ... endobj. *)
  let marker = obj_id ^ " " ^ gen_id ^ " obj" in
  if R.read f 1 <> "R" then raise Invalid;
  R.reset f;
  (* Printf.printf "looking for: %s\n%!" marker; *)
  if not (find f marker) then raise Invalid;
  let obj = R.until f "endobj" in
  (* Printf.printf "info obj: %s\n%!" obj; *)
  (* Parse the metadata object. *)
  parse_metadata obj
  |> List.map (fun (k,v) -> k, recode v)

let parse_file ?custom_parser file = R.with_file ?custom_parser parse file
