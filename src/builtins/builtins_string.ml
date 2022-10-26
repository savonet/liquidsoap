(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

let () =
  Lang.add_builtin "string" ~category:`String
    ~descr:
      "Ensure that we have a string (useful for removing fields from strings)."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      Lang.string s)

let () =
  Lang.add_module "string.base64";
  Lang.add_module "url"

let () =
  Lang.add_builtin "^" ~category:`String ~descr:"Concatenate strings."
    [("", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let s1 = Lang.to_string (Lang.assoc "" 1 p) in
      let s2 = Lang.to_string (Lang.assoc "" 2 p) in
      Lang.string (s1 ^ s2))

let () =
  Lang.add_builtin "string.concat" ~category:`String
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

let () =
  Lang.add_builtin "string.nth" ~category:`String
    ~descr:
      "Retrieve a character in a string. Raises `error.not_found` if character \
       does not exist."
    ~examples:[{|
c = string.nth("abcde", 2)
print(c) # should print "c"
|}]
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
        Runtime_error.error ~message:"string.nth: character not found!"
          "not_found")

let () =
  Lang.add_builtin "string.char" ~category:`String
    ~descr:"Create a string with one character."
    [("", Lang.int_t, None, Some "Code of the character.")]
    Lang.string_t
    (fun p ->
      List.assoc "" p |> Lang.to_int |> Char.chr |> String.make 1 |> Lang.string)

let () =
  Lang.add_builtin "string.escape" ~category:`String
    ~descr:
      "Escape special characters in an string. By default, the string is \
       assumed to be `\"utf8\"` encoded and is escaped following JSON and \
       javascript specification."
    [
      ( "special_char",
        Lang.nullable_t (Lang.fun_t [(false, "", Lang.string_t)] Lang.bool_t),
        Some Lang.null,
        Some
          "Return `true` if the given character (passed as a string) should be \
           escaped. Defaults to control charaters for `\"utf8\"` and control \
           characters and any character above `\\x7E` (non-printable \
           characters) for `\"ascii\"`." );
      ( "escape_char",
        Lang.nullable_t (Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t),
        Some Lang.null,
        Some
          "Function used to escape a character. Defaults to `\\xxx` octal \
           notation for `\"ascii\"` and `\\uxxxx` hexadecimal notation for \
           `\"utf8\"`." );
      ( "encoding",
        Lang.string_t,
        Some (Lang.string "utf8"),
        Some "One of: `\"ascii\"` or `\"utf8\"`." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      let encoding = List.assoc "encoding" p in
      let encoding : [ `Ascii | `Utf8 ] =
        match Lang.to_string encoding with
          | "ascii" -> `Ascii
          | "utf8" -> `Utf8
          | _ ->
              raise
                (Error.Invalid_value
                   ( encoding,
                     "Encoding should be one of: \"ascii\" or \"utf8\"." ))
      in
      let special_char =
        match (Lang.to_option (List.assoc "special_char" p), encoding) with
          | Some f, _ ->
              fun s ofs len ->
                Lang.to_bool
                  (Lang.apply f [("", Lang.string (String.sub s ofs len))])
          | None, `Ascii -> Utils.ascii_special_char
          | None, `Utf8 -> Utils.utf8_special_char
      in
      let escape_char =
        match (Lang.to_option (List.assoc "escape_char" p), encoding) with
          | Some f, _ ->
              fun s ofs len ->
                Lang.to_string
                  (Lang.apply f [("", Lang.string (String.sub s ofs len))])
          | None, `Ascii -> Utils.escape_hex_char
          | None, `Utf8 -> Utils.escape_utf8_char
      in
      let next =
        match encoding with
          | `Ascii -> Utils.ascii_next
          | `Utf8 -> Utils.utf8_next
      in
      try
        Lang.string
          (Utils.escape_string
             (Utils.escape ~special_char ~escape_char ~next)
             s)
      with _ ->
        let bt = Printexc.get_raw_backtrace () in
        Runtime_error.error ~bt
          ~message:
            (Printf.sprintf "Error while escaping %s string.%s"
               (match encoding with `Utf8 -> "utf8" | `Ascii -> "ascii")
               (if encoding <> `Ascii then
                " If you are not sure about the string's encoding, you should \
                 use `\"ascii\"` as this encoding never fails."
               else ""))
          "string")

let () =
  Lang.add_builtin "string.escape.all"
    ~descr:
      "Escape each character in the given string using a specific escape \
       sequence"
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
          | "octal" -> (Utils.escape_octal_char, Utils.ascii_next)
          | "hex" -> (Utils.escape_hex_char, Utils.ascii_next)
          | "utf8" -> (Utils.escape_utf8_char, Utils.utf8_next)
          | _ ->
              raise
                (Error.Invalid_value
                   ( format,
                     "Format should be one of: `\"octal\"`, `\"hex\"` or \
                      `\"utf8\"`." ))
      in
      let s = Lang.to_string (List.assoc "" p) in
      Lang.string
        (Utils.escape_string
           (Utils.escape ~special_char:(fun _ _ _ -> true) ~escape_char ~next)
           s))

let () =
  Lang.add_builtin "string.escape.special_char"
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
        | "ascii" -> Lang.bool (Utils.ascii_special_char s 0 len)
        | "utf8" -> Lang.bool (Utils.utf8_special_char s 0 len)
        | _ ->
            raise
              (Error.Invalid_value
                 (encoding, "Encoding should be one of: \"ascii\" or \"utf8\".")))

let () =
  Lang.add_builtin "string.unescape"
    ~descr:"This function is the inverse of `string.escape`." ~category:`String
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      Lang.string (Utils.unescape_string s))

let () =
  Lang.add_module "string.annotate";
  Lang.add_builtin "string.annotate.parse" ~category:`String
    ~descr:
      "Parse a string of the form `<key>=<value>,...:<uri>` as given by the \
       `annotate:` protocol"
    [("", Lang.string_t, None, None)]
    (Lang.product_t Lang.metadata_t Lang.string_t)
    (fun p ->
      let v = List.assoc "" p in
      try
        let metadata, uri = Annotate.parse (Lang.to_string v) in
        Lang.product
          (Lang.metadata (Utils.hashtbl_of_list metadata))
          (Lang.string uri)
      with Annotate.Error err ->
        raise
          (Runtime_error.Runtime_error
             {
               Runtime_error.kind = "string";
               msg = err;
               pos = (match v.Value.pos with None -> [] | Some p -> [p]);
             }))

let () =
  Lang.add_builtin "string.recode" ~category:`String
    ~descr:"Convert a string. Effective only if Camomile is enabled."
    [
      ( "in_enc",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Input encoding. Autodetected if null." );
      ( "out_enc",
        Lang.string_t,
        Some (Lang.string "UTF-8"),
        Some "Output encoding." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let in_enc =
        Lang.to_valued_option Lang.to_string (List.assoc "in_enc" p)
      in
      let out_enc = Lang.to_string (List.assoc "out_enc" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Configure.recode_tag ?in_enc ~out_enc string))

let () =
  Lang.add_builtin "string.length" ~category:`String
    ~descr:"Get the length of a string."
    [("", Lang.string_t, None, None)]
    Lang.int_t
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.int (String.length string))

let () =
  Lang.add_builtin "string.sub" ~category:`String
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
      ( "length",
        Lang.int_t,
        None,
        Some "Return a sub string of `length` characters." );
    ]
    Lang.string_t
    (fun p ->
      let start = Lang.to_int (List.assoc "start" p) in
      let len = Lang.to_int (List.assoc "length" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string
        (try String.sub string start len with Invalid_argument _ -> ""))

let () =
  Lang.add_builtin "string.case" ~category:`String
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

let () =
  Lang.add_builtin "string.trim" ~category:`String
    ~descr:"Return a string without leading and trailing whitespace."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p -> Lang.string (String.trim (Lang.to_string (List.assoc "" p))))

let () =
  Lang.add_builtin "string.capitalize" ~category:`String
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
         let l = Pcre.split ~pat:" " string in
         let l = List.map f l in
         String.concat " " l)
        else f string))

let () =
  Lang.add_builtin "string.base64.decode" ~category:`String
    ~descr:"Decode a Base64 encoded string."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      try Lang.string (Utils.decode64 string)
      with _ ->
        Runtime_error.error ~message:"Invalid base64 string!" "invalid")

let () =
  Lang.add_builtin "string.base64.encode" ~category:`String
    ~descr:"Encode a string in Base64."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Utils.encode64 string))

let () =
  Lang.add_builtin "url.decode" ~category:`String
    ~descr:"Decode an encoded url (e.g. \"%20\" becomes \" \")."
    [
      ("plus", Lang.bool_t, Some (Lang.bool true), None);
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let plus = Lang.to_bool (List.assoc "plus" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Http.url_decode ~plus string))

let () =
  Lang.add_builtin "url.encode" ~category:`String
    ~descr:"Encode an url (e.g. \" \" becomes \"%20\")."
    [
      ("plus", Lang.bool_t, Some (Lang.bool true), None);
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let plus = Lang.to_bool (List.assoc "plus" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Http.url_encode ~plus string))

let () =
  Lang.add_builtin "%" ~category:`String
    ~descr:
      "`pattern % [...,(k,v),...]` changes in the pattern occurrences of:\n\n\
       - `$(k)` into `v`\n\
       - `$(if $(k2),\"a\",\"b\") into \"a\" if k2 is found in the list, \"b\" \
       otherwise."
    [("", Lang.string_t, None, None); ("", Lang.metadata_t, None, None)]
    Lang.string_t
    (fun p ->
      let s = Lang.to_string (Lang.assoc "" 1 p) in
      let l =
        List.map
          (fun p ->
            let a, b = Lang.to_product p in
            (Lang.to_string a, Lang.to_string b))
          (Lang.to_list (Lang.assoc "" 2 p))
      in
      Lang.string (Utils.interpolate (fun k -> List.assoc k l) s))

let () =
  Lang.add_builtin "string.hex_of_int" ~category:`String
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
   * [out_value], and returns [default] in exceptional cases -- which MUST not
   * occur when default is not supplied. *)
  let register_tt doc name category func ?default in_type in_value out_value
      out_type =
    Lang.add_builtin name ~category
      ~descr:("Convert " ^ doc ^ ".")
      (let p = [("", in_type, None, None)] in
       match default with
         | None -> p
         | Some d -> ("default", out_type, Some d, None) :: p)
      out_type
      (fun p ->
        try out_value (func (in_value (List.assoc "" p)))
        with _ -> List.assoc "default" p)
  in
  let register_tts name func ~default out_value out_type =
    register_tt ("a string to a " ^ name) (name ^ "_of_string") `String func
      ~default Lang.string_t Lang.to_string out_value out_type
  in
  let register_tti name func out_value out_type =
    register_tt ("an int to a " ^ name) (name ^ "_of_int") `Math func Lang.int_t
      Lang.to_int out_value out_type
  in
  let register_ttf name func out_value out_type =
    register_tt ("a float to a " ^ name) (name ^ "_of_float") `Math func
      Lang.float_t Lang.to_float out_value out_type
  in
  register_tts "int" int_of_string ~default:(Lang.int 0)
    (fun v -> Lang.int v)
    Lang.int_t;
  register_tts "float" float_of_string ~default:(Lang.float 0.)
    (fun v -> Lang.float v)
    Lang.float_t;
  register_tts "bool" bool_of_string ~default:(Lang.bool false)
    (fun v -> Lang.bool v)
    Lang.bool_t;
  register_tti "float" float_of_int (fun v -> Lang.float v) Lang.float_t;
  register_tti "bool" (fun v -> v = 1) (fun v -> Lang.bool v) Lang.bool_t;
  register_ttf "int" int_of_float (fun v -> Lang.int v) Lang.int_t;
  register_ttf "bool" (fun v -> v = 1.) (fun v -> Lang.bool v) Lang.bool_t

let () =
  Lang.add_builtin "string_of" ~category:`String
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
      (* Always show records. *)
      let show_fields =
        if dv.Lang.value = Value.unit then true else show_fields
      in
      let v = if show_fields then v else dv in
      match v with
        | { Lang.value = Lang.(Ground (Ground.String s)); _ } -> Lang.string s
        | v -> Lang.string (Value.to_string v))

let () =
  Lang.add_builtin "string_of_float" ~category:`String
    ~descr:"String representation of a float"
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
          | Some 0 -> Printf.sprintf "%.00f" x
          | Some 1 -> Printf.sprintf "%.01f" x
          | Some 2 -> Printf.sprintf "%.02f" x
          | Some 3 -> Printf.sprintf "%.03f" x
          | Some 4 -> Printf.sprintf "%.04f" x
          | _ -> string_of_float x
      in
      Lang.string s)

let () =
  Lang.add_builtin "string.id" ~category:`String
    ~descr:"Generate an identifier with given operator name."
    [("", Lang.string_t, None, Some "Operator name.")]
    Lang.string_t
    (fun p ->
      let name = List.assoc "" p |> Lang.to_string in
      Lang.string (Source.generate_id name))

let () =
  Lang.add_builtin "string.make" ~category:`String
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
        Runtime_error.error ~message:"Invalid string length!" "invalid";
      let c =
        try Char.chr (Lang.to_int (List.assoc "char_code" p))
        with _ ->
          Runtime_error.error ~message:"Invalid character code!" "invalid"
      in
      Lang.string (String.make n c))

let () =
  Lang.add_module "string.apic";
  let t =
    Lang.method_t Lang.string_t
      [
        ("mime", ([], Lang.string_t), "Mime type");
        ("picture_type", ([], Lang.int_t), "Picture type");
        ("description", ([], Lang.string_t), "Description");
      ]
  in
  Lang.add_builtin "string.apic.parse" ~category:`File
    [("", Lang.string_t, None, Some "APIC data.")]
    t
    ~descr:
      "Parse APIC ID3v2 tags (such as those obtained in the APIC tag from \
       `file.metadata.id3v2`). The returned values are: mime, picture type, \
       description, and picture data."
    (fun p ->
      let apic = Lang.to_string (List.assoc "" p) in
      let apic = Metadata.ID3v2.parse_apic apic in
      Lang.meth
        (Lang.string apic.Metadata.ID3v2.data)
        [
          ("mime", Lang.string apic.Metadata.ID3v2.mime);
          ("picture_type", Lang.int apic.Metadata.ID3v2.picture_type);
          ("description", Lang.string apic.Metadata.ID3v2.description);
        ])
