(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

let log = Log.make ["lang"; "string"]

let conf_string =
  Dtools.Conf.void ~p:(Configure.conf#plug "string") "String settings"

let () =
  let conf_default_encoding =
    Dtools.Conf.string
      ~p:(conf_string#plug "default_encoding")
      ~d:"utf8"
      "Default encoding for `string.length`, `string.chars` and `string.sub`"
  in
  conf_default_encoding#on_change (fun v ->
      let enc =
        match v with
          | "ascii" -> `Ascii
          | "utf8" -> `Utf8
          | _ ->
              log#important
                "Invalid value %s for `settings.string.default_encoding`! \
                 Should be one of: \"ascii\" or \"utf8\"."
                v;
              `Utf8
      in
      Liquidsoap_lang.Builtins_string.default_encoding := enc)

let string = Liquidsoap_lang.Builtins_string.string
let string_annotate = Lang.add_module ~base:string "annotate"

let _ =
  Lang.add_builtin ~base:string_annotate "parse" ~category:`String
    ~descr:
      "Parse a string of the form `<key>=<value>,...:<uri>` as given by the \
       `annotate:` protocol"
    [("", Lang.string_t, None, None)]
    (Lang.product_t Lang.metadata_t Lang.string_t)
    (fun p ->
      let v = List.assoc "" p in
      try
        let metadata, uri = Annotate_parser.parse (Lang.to_string v) in
        Lang.product
          (Lang.metadata (Frame.Metadata.from_list metadata))
          (Lang.string uri)
      with Annotate_parser.Error err ->
        Lang.raise_error ~message:err ~pos:(Lang.pos p) "string")

let _ =
  Lang.add_builtin ~base:string "recode" ~category:`String
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
      try
        let in_enc =
          List.assoc "in_enc" p
          |> Lang.to_valued_option Lang.to_string
          |> Option.map Charset.of_string
        in
        let out_enc =
          List.assoc "out_enc" p |> Lang.to_string |> Charset.of_string
        in
        let string = Lang.to_string (List.assoc "" p) in
        Lang.string (Charset.convert ?source:in_enc ~target:out_enc string)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"string" exn)

let _ =
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

let string_apic = Lang.add_module ~base:string "apic"

let _ =
  let t =
    Lang.method_t Lang.string_t
      [
        ("mime", ([], Lang.string_t), "Mime type");
        ("picture_type", ([], Lang.int_t), "Picture type");
        ("description", ([], Lang.string_t), "Description");
      ]
  in
  Lang.add_builtin ~base:string_apic "parse" ~category:`Metadata
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

let string_pic = Lang.add_module ~base:string "pic"

let _ =
  let t =
    Lang.method_t Lang.string_t
      [
        ("format", ([], Lang.string_t), "Picture format");
        ("picture_type", ([], Lang.int_t), "Picture type");
        ("description", ([], Lang.string_t), "Description");
      ]
  in
  Lang.add_builtin ~base:string_pic "parse" ~category:`Metadata
    [("", Lang.string_t, None, Some "PIC data.")]
    t
    ~descr:
      "Parse PIC ID3v2 tags (such as those obtained in the PIC tag from \
       `file.metadata.id3v2`). The returned values are: format, picture type, \
       description, and picture data."
    (fun p ->
      let pic = Lang.to_string (List.assoc "" p) in
      let pic = Metadata.ID3v2.parse_pic pic in
      Lang.meth
        (Lang.string pic.Metadata.ID3v2.pic_data)
        [
          ("format", Lang.string pic.Metadata.ID3v2.pic_format);
          ("picture_type", Lang.int pic.Metadata.ID3v2.pic_type);
          ("description", Lang.string pic.Metadata.ID3v2.pic_description);
        ])

let _ =
  Lang.add_builtin ~base:string "id" ~category:`String
    ~descr:"Generate an identifier with given operator name."
    [
      ("category", Lang.string_t, Some (Lang.string ""), Some "Category");
      ("", Lang.string_t, None, Some "Operator name.");
    ]
    Lang.string_t
    (fun p ->
      let name = List.assoc "" p |> Lang.to_string in
      let category = List.assoc "category" p |> Lang.to_string in
      Lang.string (Lang_string.generate_id ~category name))
