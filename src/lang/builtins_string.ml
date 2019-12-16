(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Lang_builtins

let () =
  add_builtin "^" ~cat:String ~descr:"Concatenate strings."
    [("", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.string_t (fun p ->
      let s1 = Lang.to_string (Lang.assoc "" 1 p) in
      let s2 = Lang.to_string (Lang.assoc "" 2 p) in
      Lang.string (s1 ^ s2))

let () =
  add_builtin "string.concat" ~cat:String ~descr:"Concatenate strings."
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
  add_builtin "string.nth" ~cat:String
    ~descr:"Retrieve a character in a string."
    [
      ("", Lang.string_t, None, Some "String to look into.");
      ("", Lang.int_t, None, Some "Index of the character.");
    ] Lang.int_t (fun p ->
      let s = Lang.to_string (Lang.assoc "" 1 p) in
      let n = Lang.to_int (Lang.assoc "" 2 p) in
      Lang.int (int_of_char s.[n]))

(* Special characters that must be escaped *)
let special_chars =
  (* Control chars between 0x00 and 0x1f *)
  let rec escaped p l =
    if p <= 0x1f then escaped (p + 1) (Char.chr p :: l) else List.rev l
  in
  (* We also add 0x7F (DEL) and the
   * usual '"' and '\' *)
  escaped 0 ['"'; '\\'; '\x7F']

let register_escape_fun ~name ~descr ~escape ~escape_char =
  let escape ~special_char ~escape_char s =
    let b = Buffer.create (String.length s) in
    let f = Format.formatter_of_buffer b in
    escape ~special_char ~escape_char f s;
    Format.pp_print_flush f ();
    Buffer.contents b
  in
  let special_chars =
    Lang.list ~t:Lang.string_t
      (List.map Lang.string (List.map (String.make 1) special_chars))
  in
  let escape_char p _ =
    let v = List.assoc "" p in
    Lang.string (escape_char (Lang.to_string v).[0])
  in
  let escape_char =
    Lang.val_fun
      [("", "", Lang.string_t, None)]
      ~ret_t:Lang.string_t escape_char
  in
  add_builtin name ~cat:String ~descr
    [
      ( "special_chars",
        Lang.list_t Lang.string_t,
        Some special_chars,
        Some
          "List of characters that should be escaped. The first character of \
           each element in the list is considered." );
      ( "escape_char",
        Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t,
        Some escape_char,
        Some "Function used to escape a character." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      let special_chars =
        List.map
          (fun s -> s.[0])
          (List.map Lang.to_string
             (Lang.to_list (List.assoc "special_chars" p)))
      in
      let special_char c = List.mem c special_chars in
      let f = List.assoc "escape_char" p in
      let escape_char c =
        Lang.to_string
          (Lang.apply f ~t:Lang.string_t [("", Lang.string (String.make 1 c))])
      in
      Lang.string (escape ~special_char ~escape_char s))

let () =
  let escape ~special_char ~escape_char f s =
    Utils.escape ~special_char ~escape_char f s
  in
  register_escape_fun ~name:"string.escape"
    ~descr:
      "Escape special characters in a string. String is parsed char by char. \
       See `string.utf8.escape` for an UTF8-aware parsing function."
    ~escape ~escape_char:Utils.escape_char;
  let escape ~special_char ~escape_char f s =
    Utils.escape_utf8 ~special_char ~escape_char f s
  in
  register_escape_fun ~name:"string.utf8.escape"
    ~descr:"Escape special charaters in an UTF8 string." ~escape
    ~escape_char:Utils.escape_utf8_char

let () =
  add_builtin "string.split" ~cat:String
    ~descr:
      "Split a string at 'separator'. \n\
       Perl compatible regular expressions are recognized. Hence, special \
       characters should be escaped."
    [("separator", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    (Lang.list_t Lang.string_t) (fun p ->
      let sep = Lang.to_string (List.assoc "separator" p) in
      let string = Lang.to_string (List.assoc "" p) in
      let rex = Pcre.regexp sep in
      Lang.list ~t:Lang.string_t (List.map Lang.string (Pcre.split ~rex string)))

let () =
  add_builtin "string.extract" ~cat:String
    ~descr:
      "Extract substrings from a string. \n\
       Perl compatible regular expressions are recognized. Hence, special \
       characters should be escaped. \n\
       Returns a list of (index,value).\n\
       If the list does not have a pair associated to some index, it means \
       that the corresponding pattern was not found."
    [("pattern", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.metadata_t (fun p ->
      let pattern = Lang.to_string (List.assoc "pattern" p) in
      let string = Lang.to_string (List.assoc "" p) in
      let rex = Pcre.regexp pattern in
      try
        let sub = Pcre.exec ~rex string in
        let n = Pcre.num_of_subs sub in
        let rec extract l i =
          if i < n then (
            try
              extract (l @ [(string_of_int i, Pcre.get_substring sub i)]) (i + 1)
            with Not_found -> extract l (i + 1) )
          else l
        in
        let l = extract [] 1 in
        Lang.list
          ~t:(Lang.product_t Lang.string_t Lang.string_t)
          (List.map
             (fun (x, y) -> Lang.product (Lang.string x) (Lang.string y))
             l)
      with Not_found ->
        Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) [])

let () =
  add_builtin "string.match" ~cat:String
    ~descr:
      "Match a string with an expression. Perl compatible regular expressions \
       are recognized. Hence, special characters should be escaped."
    [("pattern", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.bool_t (fun p ->
      let pattern = Lang.to_string (List.assoc "pattern" p) in
      let string = Lang.to_string (List.assoc "" p) in
      let rex = Pcre.regexp pattern in
      Lang.bool (Pcre.pmatch ~rex string))

let () =
  add_builtin "string.recode" ~cat:String
    ~descr:"Convert a string. Effective only if Camomile is enabled."
    [
      ( "in_enc",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Input encoding. Autodetected if empty." );
      ( "out_enc",
        Lang.string_t,
        Some (Lang.string "UTF-8"),
        Some "Output encoding." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let in_enc =
        match Lang.to_string (List.assoc "in_enc" p) with
          | "" -> None
          | s -> Some s
      in
      let out_enc = Lang.to_string (List.assoc "out_enc" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Configure.recode_tag ?in_enc ~out_enc string))

let () =
  add_builtin "string.length" ~cat:String ~descr:"Get the length of a string."
    [("", Lang.string_t, None, None)] Lang.int_t (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.int (String.length string))

let () =
  add_builtin "string.sub" ~cat:String
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
    ] Lang.string_t (fun p ->
      let start = Lang.to_int (List.assoc "start" p) in
      let len = Lang.to_int (List.assoc "length" p) in
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string
        (try String.sub string start len with Invalid_argument _ -> ""))

let () =
  add_builtin "string.case" ~cat:String
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
        ( if lower then String.lowercase_ascii string
        else String.uppercase_ascii string ))

let () =
  add_builtin "string.trim" ~cat:String
    ~descr:"Return a string without leading and trailing whitespace."
    [("", Lang.string_t, None, None)] Lang.string_t (fun p ->
      Lang.string (String.trim (Lang.to_string (List.assoc "" p))))

let () =
  add_builtin "string.capitalize" ~cat:String
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
        ( if space_sensitive then (
          let l = Pcre.split ~pat:" " string in
          let l = List.map f l in
          String.concat " " l )
        else f string ))

let () =
  add_builtin "string.replace" ~cat:String
    ~descr:
      "Replace substrings in a string. \n\
       Will replace all substrings matched in the pattern by the string \
       returned by the replace function."
    [
      ("pattern", Lang.string_t, None, None);
      ("", Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t, None, None);
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let pattern = Lang.to_string (List.assoc "pattern" p) in
      let string = Lang.to_string (Lang.assoc "" 2 p) in
      let subst = Lang.assoc "" 1 p in
      let subst s =
        let ret = Lang.apply ~t:Lang.string_t subst [("", Lang.string s)] in
        Lang.to_string ret
      in
      let rex = Pcre.regexp pattern in
      Lang.string (Pcre.substitute ~rex ~subst string))

let () =
  add_builtin "string.base64.decode" ~cat:String
    ~descr:"Decode a Base64 encoded string." [("", Lang.string_t, None, None)]
    Lang.string_t (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Utils.decode64 string))

let () =
  add_builtin "string.base64.encode" ~cat:String
    ~descr:"Encode a string in Base64." [("", Lang.string_t, None, None)]
    Lang.string_t (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.string (Utils.encode64 string))

let () =
  add_builtin "url.decode" ~cat:String
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
  add_builtin "url.encode" ~cat:String
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
  add_builtin "%" ~cat:String
    ~descr:
      "`pattern % [...,(k,v),...]` changes in the pattern occurences of:\n\n\
       - `$(k)` into `v`\n\
       - `$(if $(k2),\"a\",\"b\") into \"a\" if k2 is found in the list, \"b\" \
       otherwise."
    [("", Lang.string_t, None, None); ("", Lang.metadata_t, None, None)]
    Lang.string_t (fun p ->
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
  add_builtin "string.quote" ~cat:String ~descr:"Escape shell metacharacters."
    [("", Lang.string_t, None, None)] Lang.string_t (fun p ->
      let s = Lang.to_string (List.assoc "" p) in
      Lang.string (Utils.quote s))

(** Data conversions. *)

let () =
  (* Register as [name] the function which composes [in_value],[func] and
   * [out_value], and returns [default] in exceptional cases -- which MUST not
   * occur when default is not supplied. *)
  let register_tt doc name cat func ?default in_type in_value out_value out_type
      =
    add_builtin name ~cat
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
    register_tt ("a string to a " ^ name) (name ^ "_of_string") String func
      ~default Lang.string_t Lang.to_string out_value out_type
  in
  let register_tti name func out_value out_type =
    register_tt ("an int to a " ^ name) (name ^ "_of_int") Math func Lang.int_t
      Lang.to_int out_value out_type
  in
  let register_ttf name func out_value out_type =
    register_tt ("a float to a " ^ name) (name ^ "_of_float") Math func
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
  add_builtin "string_of" ~cat:String
    ~descr:"Return the representation of a value."
    [("", Lang.univ_t (), None, None)]
    Lang.string_t
    (fun p ->
      match List.assoc "" p with
        | { Lang.value = Lang.String s; _ } -> Lang.string s
        | v -> Lang.string (Lang.print_value v))
