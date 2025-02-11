let string =
  Lang.add_builtin "string" ~category:`String
    ~descr:"Return the representation of a value."
    [
      ( "fields",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Show toplevel fields around the value." );
      ("", Lang.univ_t (), None, None);
    ]
    Lang.string_t
    (fun p ->
      let show_fields = Lang.to_bool (List.assoc "fields" p) in
      let v = List.assoc "" p in
      let dv = Lang.demeth v in
      (* Always show fields for records. *)
      let show_fields = if Value.is_unit dv then true else show_fields in
      let v = if show_fields then v else dv in
      match v with
        | String { value = s } -> Lang.string s
        | v -> Lang.string (Value.to_string v))

let _ =
  Lang.add_builtin "^" ~category:`String ~descr:"Concatenate strings."
    [("", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let s1 = Lang.to_string (Lang.assoc "" 1 p) in
      let s2 = Lang.to_string (Lang.assoc "" 2 p) in
      Lang.string (s1 ^ s2))

let _ =
  Lang.add_builtin ~base:string "compare" ~category:`String
    ~descr:"Compare strings in lexicographical order."
    [("", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.int_t
    (fun p ->
      let s1 = Lang.to_string (Lang.assoc "" 1 p) in
      let s2 = Lang.to_string (Lang.assoc "" 2 p) in
      Lang.int (String.compare s1 s2))

let _ =
  Lang.add_builtin ~base:string "digest" ~category:`String
    ~descr:"Return an MD5 digest for the given string."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let data = Lang.to_string (List.assoc "" p) in
      Lang.string Digest.(to_hex (string data)))

let _ =
  Lang.add_builtin ~base:string "concat" ~category:`String
    ~descr:"Concatenate strings."
    [
      ("separator", Lang.string_t, Some (Lang.string ""), None);
      ("", Lang.list_t Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let sep = Lang.to_string (List.assoc "separator" p) in
      let l = Lang.to_list (List.assoc "" p) in
      let l = List.map Lang.to_string l in
      Lang.string (String.concat sep l))

let split ~encoding s =
  let buf = Buffer.create 1 in
  let to_string add c =
    Buffer.clear buf;
    add buf c;
    Buffer.contents buf
  in
  let get =
    match encoding with
      | `Ascii -> fun pos -> (to_string Buffer.add_char (String.get s pos), 1)
      | `Utf8 ->
          fun pos ->
            let d = String.get_utf_8_uchar s pos in
            if not (Uchar.utf_decode_is_valid d) then
              failwith "Decoding failed!";
            ( to_string Buffer.add_utf_8_uchar (Uchar.utf_decode_uchar d),
              Uchar.utf_decode_length d )
  in
  let len = String.length s in
  let rec f chars pos =
    if pos = len then List.rev chars
    else (
      let char, len = get pos in
      f (char :: chars) (pos + len))
  in
  f [] 0

let default_encoding = ref `Utf8

let encoding_option =
  ( "encoding",
    Lang.nullable_t Lang.string_t,
    Some Lang.null,
    Some
      "Encoding used to split characters. Should be one of: `\"utf8\"` or \
       `\"ascii\"`" )

let get_encoding p =
  match Lang.to_valued_option Lang.to_string (List.assoc "encoding" p) with
    | None -> ("utf8", !default_encoding)
    | Some "utf8" -> ("utf8", `Utf8)
    | Some "ascii" -> ("ascii", `Ascii)
    | _ ->
        Runtime_error.raise ~pos:(Lang.pos p) ~message:"Invalid encoding!"
          "invalid"

let _ =
  Lang.add_builtin ~base:string "chars" ~category:`String
    ~descr:"Split string into characters. Raises `error.invalid` on errors."
    [encoding_option; ("", Lang.string_t, None, None)]
    (Lang.list_t Lang.string_t)
    (fun p ->
      let enc, encoding = get_encoding p in
      let s = Lang.to_string (List.assoc "" p) in
      try Lang.list (List.map Lang.string (split ~encoding s))
      with _ ->
        Runtime_error.raise ~pos:(Lang.pos p)
          ~message:
            (Printf.sprintf "String cannot be split using encoding `\"%s\"`!"
               enc)
          "invalid")

let _ =
  Lang.add_builtin ~base:string "length" ~category:`String
    ~descr:
      "Return the string's length using the given encoding. Raises \
       `error.invalid` on errors."
    [encoding_option; ("", Lang.string_t, None, None)]
    Lang.int_t
    (fun p ->
      let enc, encoding = get_encoding p in
      let s = Lang.to_string (List.assoc "" p) in
      try Lang.int (List.length (split ~encoding s))
      with _ ->
        Runtime_error.raise ~pos:(Lang.pos p)
          ~message:
            (Printf.sprintf "String cannot be split using encoding `\"%s\"`!"
               enc)
          "invalid")

let _ =
  Lang.add_builtin ~base:string "nth" ~category:`String
    ~descr:
      "Retrieve a character in a string. Raises `error.not_found` if character \
       does not exist."
    ~examples:
      [
        {|
c = string.nth("abcde", 2)
print(c) # should print 99 which is the ascii code for "c"
|};
      ]
    [
      ("", Lang.string_t, None, Some "String to look into.");
      ("", Lang.int_t, None, Some "Index of the character.");
    ]
    Lang.int_t
    (fun p ->
      try
        let s = Lang.to_string (Lang.assoc "" 1 p) in
        let n = Lang.to_int (Lang.assoc "" 2 p) in
        Lang.int (int_of_char s.[n])
      with _ ->
        Runtime_error.raise ~pos:(Lang.pos p)
          ~message:"string.nth: character not found!" "not_found")

let _ =
  Lang.add_builtin ~base:string "char" ~category:`String
    ~descr:"Create a string with one character."
    [("", Lang.int_t, None, Some "Code of the character.")]
    Lang.string_t
    (fun p ->
      List.assoc "" p |> Lang.to_int |> Char.chr |> String.make 1 |> Lang.string)

let string_escape =
  Lang.add_builtin ~base:string "escape" ~category:`String
    ~descr:
      "Escape special characters in an string. By default, the string is \
       assumed to be `\"utf8\"` encoded and is escaped following JSON and \
       javascript specification."
    [
      ( "special_char",
        Lang.nullable_t
          (Lang.fun_t
             [(false, "encoding", Lang.string_t); (false, "", Lang.string_t)]
             Lang.bool_t),
        Some Lang.null,
        Some
          "Return `true` if the given character (passed as a string) should be \
           escaped. Defaults to control characters for `\"utf8\"` and control \
           characters and any character above `\\x7E` (non-printable \
           characters) for `\"ascii\"`." );
      ( "escape_char",
        Lang.nullable_t
          (Lang.fun_t
             [(false, "encoding", Lang.string_t); (false, "", Lang.string_t)]
             Lang.string_t),
        Some Lang.null,
        Some
          "Function used to escape a character. Defaults to `\\xxx` octal \
           notation for `\"ascii\"` and `\\uxxxx` hexadecimal notation for \
           `\"utf8\"`." );
      ( "encoding",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "One of: `\"ascii\"` or `\"utf8\"`. If `null`, `utf8` is tried first \
           and `ascii` is used as a fallback if this fails." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      let encoding = List.assoc "encoding" p in
      let encoding : [ `Default | `Ascii | `Utf8 ] =
        match Lang.to_valued_option Lang.to_string encoding with
          | None -> `Default
          | Some "ascii" -> `Ascii
          | Some "utf8" -> `Utf8
          | Some _ ->
              raise
                (Error.Invalid_value
                   ( encoding,
                     "Encoding should be one of: \"ascii\" or \"utf8\"." ))
      in
      let exec encoding =
        let encoding_string =
          match encoding with `Utf8 -> "utf8" | `Ascii -> "ascii"
        in
        let special_char =
          match (Lang.to_option (List.assoc "special_char" p), encoding) with
            | Some f, _ ->
                fun s ofs len ->
                  Lang.to_bool
                    (Lang.apply f
                       [
                         ("encoding", Lang.string encoding_string);
                         ("", Lang.string (String.sub s ofs len));
                       ])
            | None, `Ascii -> Lang_string.ascii_special_char
            | None, `Utf8 -> Lang_string.utf8_special_char
        in
        let escape_char =
          match (Lang.to_option (List.assoc "escape_char" p), encoding) with
            | Some f, _ ->
                fun s ofs len ->
                  Lang.to_string
                    (Lang.apply f
                       [
                         ("encoding", Lang.string encoding_string);
                         ("", Lang.string (String.sub s ofs len));
                       ])
            | None, `Ascii -> Lang_string.escape_hex_char
            | None, `Utf8 -> Lang_string.escape_utf8_char ~strict:false
        in
        let next =
          match encoding with
            | `Ascii -> Lang_string.ascii_next
            | `Utf8 -> Lang_string.utf8_next
        in
        try
          Lang.string
            (Lang_string.escape_string
               (Lang_string.escape ~special_char ~escape_char ~next)
               s)
        with _ ->
          let bt = Printexc.get_raw_backtrace () in
          Runtime_error.raise ~bt ~pos:(Lang.pos p)
            ~message:
              (Printf.sprintf "Error while escaping %s string.%s"
                 (match encoding with `Utf8 -> "utf8" | `Ascii -> "ascii")
                 (if encoding <> `Ascii then
                    " If you are not sure about the string's encoding, you \
                     should use `\"ascii\"` as this encoding never fails."
                  else ""))
            "string"
      in
      match encoding with
        | `Default -> ( try exec `Utf8 with _ -> exec `Ascii)
        | (`Ascii | `Utf8) as encoding -> exec encoding)

let _ =
  Lang.add_builtin ~base:string_escape "all"
    ~descr:
      "Escape each character in the given string using a specific escape \
       sequence."
    ~category:`String
    [
      ( "format",
        Lang.string_t,
        Some (Lang.string "utf8"),
        Some "Escape format. One of: `\"octal\"`, `\"hex\"` or `\"utf8\"`." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let format = List.assoc "format" p in
      let escape_char, next =
        match Lang.to_string format with
          | "octal" -> (Lang_string.escape_octal_char, Lang_string.ascii_next)
          | "hex" -> (Lang_string.escape_hex_char, Lang_string.ascii_next)
          | "utf8" ->
              (Lang_string.escape_utf8_char ~strict:false, Lang_string.utf8_next)
          | _ ->
              raise
                (Error.Invalid_value
                   ( format,
                     "Format should be one of: `\"octal\"`, `\"hex\"` or \
                      `\"utf8\"`." ))
      in
      let s = Lang.to_string (List.assoc "" p) in
      Lang.string
        (Lang_string.escape_string
           (Lang_string.escape
              ~special_char:(fun _ _ _ -> true)
              ~escape_char ~next)
           s))

let _ =
  Lang.add_builtin ~base:string_escape "special_char"
    ~descr:
      "Default function to detect characters to escape. See `string.escape` \
       for more details."
    ~category:`String
    [
      ( "encoding",
        Lang.string_t,
        Some (Lang.string "utf8"),
        Some "One of: `\"ascii\"` or `\"utf8\"`." );
      ("", Lang.string_t, None, None);
    ]
    Lang.bool_t
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      let len = String.length s in
      let encoding = List.assoc "encoding" p in
      match Lang.to_string encoding with
        | "ascii" -> Lang.bool (Lang_string.ascii_special_char s 0 len)
        | "utf8" -> Lang.bool (Lang_string.utf8_special_char s 0 len)
        | _ ->
            raise
              (Error.Invalid_value
                 (encoding, "Encoding should be one of: \"ascii\" or \"utf8\".")))

let _ =
  Lang.add_builtin ~base:string "unescape"
    ~descr:"This function is the inverse of `string.escape`." ~category:`String
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      Lang.string (Lang_string.unescape_string s))

let _ =
  Lang.add_builtin ~base:string "sub" ~category:`String
    ~descr:
      "Get a substring of a string. Returns \"\" if no such substring exists."
    [
      ("", Lang.string_t, None, None);
      ( "start",
        Lang.int_t,
        None,
        Some
          "Return a sub string starting at this position. First position is 0."
      );
      encoding_option;
      ( "length",
        Lang.int_t,
        None,
        Some "Return a sub string of `length` characters." );
    ]
    Lang.string_t
    (fun p ->
      let start = Lang.to_int (List.assoc "start" p) in
      let len = Lang.to_int (List.assoc "length" p) in
      let _, encoding = get_encoding p in
      let string = Lang.to_string (List.assoc "" p) in
      let s =
        match encoding with
          | `Ascii -> (
              try String.sub string start len with Invalid_argument _ -> "")
          | `Utf8 -> (
              try
                let chars = split ~encoding string in
                if List.length chars < len + start then ""
                else
                  String.concat ""
                    (List.filteri
                       (fun pos _ -> start <= pos && pos < start + len)
                       chars)
              with _ -> "")
      in
      Lang.string s)

let _ =
  Lang.add_builtin ~base:string "index" ~category:`String
    ~descr:
      "Index where a substring occurs in a string. The function returns `-1` \
       if the substring is not present"
    [
      ("substring", Lang.string_t, None, Some "Substring to look for.");
      ("", Lang.string_t, None, Some "String in which to look.");
    ]
    Lang.int_t
    (fun p ->
      let t = List.assoc "substring" p |> Lang.to_string in
      let s = List.assoc "" p |> Lang.to_string in
      let ans =
        let m = String.length t in
        let n = String.length s in
        let ans = ref (-1) in
        try
          for i = 0 to n - m do
            if String.sub s i m = t then (
              ans := i;
              raise Exit)
          done;
          -1
        with Exit -> !ans
      in
      Lang.int ans)

let _ =
  Lang.add_builtin ~base:string "case" ~category:`String
    ~descr:"Convert a string to lower or upper case."
    [
      ( "lower",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Convert to lower case if true and uppercase otherwise." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let lower = Lang.to_bool (List.assoc "lower" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string
        (if lower then String.lowercase_ascii string
         else String.uppercase_ascii string))

let _ =
  Lang.add_builtin ~base:string "trim" ~category:`String
    ~descr:"Return a string without leading and trailing whitespace."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p -> Lang.string (String.trim (Lang.to_string (List.assoc "" p))))

let _ =
  Lang.add_builtin ~base:string "capitalize" ~category:`String
    ~descr:
      "Return a string with the first character set to upper case \
       (capitalize), or to lower case (uncapitalize)."
    [
      ( "capitalize",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Capitalize if true, uncapitalize otherwise" );
      ( "space_sensitive",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Capitalize each space separated sub-string." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let cap = Lang.to_bool (List.assoc "capitalize" p) in
      let space_sensitive = Lang.to_bool (List.assoc "space_sensitive" p) in
      let string = Lang.to_string (List.assoc "" p) in
      let f s =
        if cap then String.capitalize_ascii s else String.uncapitalize_ascii s
      in
      Lang.string
        (if space_sensitive then (
           let l = String.split_on_char ' ' string in
           let l = List.map f l in
           String.concat " " l)
         else f string))

let _ =
  Lang.add_builtin ~base:string "hex_of_int" ~category:`String
    ~descr:"Hexadecimal representation of an integer."
    [
      ( "pad",
        Lang.int_t,
        Some (Lang.int 0),
        Some
          "Minimum length in digits (pad on the left with zeros in order to \
           reach it)." );
      ("", Lang.int_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let pad = Lang.to_int (List.assoc "pad" p) in
      let n = Lang.to_int (List.assoc "" p) in
      let s = Printf.sprintf "%x" n in
      let s =
        let len = String.length s in
        if len < pad then String.make (pad - len) '0' ^ s else s
      in
      Lang.string s)

(** Data conversions. *)

let () =
  (* Register as [name] the function which composes [in_value],[func] and
     [out_value], and returns [default] in exceptional cases -- which MUST not
     occur when default is not supplied. *)
  let register_tt doc name category func ~needs_default in_type in_value
      out_value out_type =
    let raise_doc =
      if needs_default then
        [%string
          {| Raises `error.failure("%{doc}")` if conversion fails and default is `null`|}]
      else ""
    in
    ignore
      (Lang.add_builtin name ~category
         ~descr:[%string {|Convert %{doc}.%{raise_doc}|}]
         ([("", in_type, None, None)]
         @
         if needs_default then
           [("default", Lang.nullable_t out_type, Some Lang.null, None)]
         else [])
         out_type
         (fun p ->
           try out_value (func (in_value (List.assoc "" p)))
           with _ -> (
             try Option.get (Lang.to_option (List.assoc "default" p))
             with _ ->
               Runtime_error.raise ~pos:(Lang.pos p) ~message:name "failure")))
  in
  let register_tts name func out_value out_type =
    register_tt ~needs_default:true ("a string to a " ^ name)
      (name ^ "_of_string") `String func Lang.string_t Lang.to_string out_value
      out_type
  in
  let register_tti name func out_value out_type =
    register_tt ~needs_default:false ("an int to a " ^ name) (name ^ "_of_int")
      `Math func Lang.int_t Lang.to_int out_value out_type
  in
  let register_ttf name func out_value out_type =
    register_tt ~needs_default:false ("a float to a " ^ name)
      (name ^ "_of_float") `Math func Lang.float_t Lang.to_float out_value
      out_type
  in
  register_tts "int" int_of_string (fun v -> Lang.int v) Lang.int_t;
  register_tts "float" float_of_string (fun v -> Lang.float v) Lang.float_t;
  register_tts "bool" bool_of_string (fun v -> Lang.bool v) Lang.bool_t;
  register_tti "float" float_of_int (fun v -> Lang.float v) Lang.float_t;
  register_tti "bool" (fun v -> v = 1) (fun v -> Lang.bool v) Lang.bool_t;
  register_ttf "int" int_of_float (fun v -> Lang.int v) Lang.int_t;
  register_ttf "bool" (fun v -> v = 1.) (fun v -> Lang.bool v) Lang.bool_t

let _ =
  Lang.add_builtin ~base:string "float" ~category:`String
    ~descr:"String representation of a float."
    [
      ( "decimal_places",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some "Number of decimal places." );
      ("", Lang.float_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let dp =
        List.assoc "decimal_places" p
        |> Lang.to_option |> Option.map Lang.to_int
      in
      let x = List.assoc "" p |> Lang.to_float in
      let s =
        match dp with
          | Some d -> Printf.sprintf "%.*f" d x
          | None -> Utils.string_of_float x
      in
      Lang.string s)

let _ =
  Lang.add_builtin ~base:string "make" ~category:`String
    ~descr:"Create a string of a given length using the given character."
    [
      ( "char_code",
        Lang.int_t,
        Some (Lang.int (Char.code ' ')),
        Some "Character code." );
      ("", Lang.int_t, None, Some "String length.");
    ]
    Lang.string_t
    (fun p ->
      let n = Lang.to_int (List.assoc "" p) in
      if n < 0 then
        Runtime_error.raise ~pos:(Lang.pos p) ~message:"Invalid string length!"
          "invalid";
      let c =
        try Char.chr (Lang.to_int (List.assoc "char_code" p))
        with _ ->
          Runtime_error.raise ~pos:(Lang.pos p)
            ~message:"Invalid character code!" "invalid"
      in
      Lang.string (String.make n c))

let _ =
  Lang.add_builtin ~base:string "id" ~category:`String
    ~descr:"Generate an identifier with given operator name."
    [("", Lang.string_t, None, Some "Operator name.")]
    Lang.string_t
    (fun p ->
      let name = List.assoc "" p |> Lang.to_string in
      Lang.string (Lang_string.generate_id name))

let string_base64 = Lang.add_module ~base:string "base64"

let _ =
  Lang.add_builtin ~base:string_base64 "decode" ~category:`String
    ~descr:"Decode a Base64 encoded string."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      try Lang.string (Lang_string.decode64 string)
      with _ ->
        Runtime_error.raise ~pos:(Lang.pos p) ~message:"Invalid base64 string!"
          "invalid")

let _ =
  Lang.add_builtin ~base:string_base64 "encode" ~category:`String
    ~descr:"Encode a string in Base64."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Lang_string.encode64 string))

let url = Modules.url

let _ =
  Lang.add_builtin ~base:url "decode" ~category:`String
    ~descr:"Decode an encoded url (e.g. \"%20\" becomes \" \")."
    [
      ("plus", Lang.bool_t, Some (Lang.bool true), None);
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let plus = Lang.to_bool (List.assoc "plus" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Lang_string.url_decode ~plus string))

let _ =
  Lang.add_builtin ~base:url "encode" ~category:`String
    ~descr:"Encode an url (e.g. \" \" becomes \"%20\")."
    [
      ("plus", Lang.bool_t, Some (Lang.bool true), None);
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let plus = Lang.to_bool (List.assoc "plus" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Lang_string.url_encode ~plus string))
