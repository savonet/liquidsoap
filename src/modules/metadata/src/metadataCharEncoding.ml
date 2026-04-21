type recode =
  ?source:[ `ISO_8859_1 | `UTF_8 | `UTF_16 | `UTF_16LE | `UTF_16BE ] ->
  ?target:[ `UTF_8 | `UTF_16 | `UTF_16LE | `UTF_16BE ] ->
  string ->
  string

module type T = sig
  val convert :
    ?source:[ `ISO_8859_1 | `UTF_8 | `UTF_16 | `UTF_16LE | `UTF_16BE ] ->
    ?target:[ `UTF_8 | `UTF_16 | `UTF_16LE | `UTF_16BE ] ->
    string ->
    string
end

module Naive : T = struct
  let convert ?(source = `UTF_8) ?(target = `UTF_8) s =
    match (source, target) with
      | `UTF_8, `UTF_8
      | `UTF_16, `UTF_16
      | `UTF_16LE, `UTF_16LE
      | `UTF_16BE, `UTF_16BE
      | _ ->
          let buf = Buffer.create 10 in
          let len = String.length s in
          let add_unicode_char =
            match target with
              | `UTF_8 -> Buffer.add_utf_8_uchar
              | `UTF_16BE -> Buffer.add_utf_16be_uchar
              | _ -> Buffer.add_utf_16le_uchar
          in
          let unicode_byte_length =
            match source with
              | `ISO_8859_1 -> fun _ -> 1
              | `UTF_8 -> Uchar.utf_8_byte_length
              | _ -> Uchar.utf_16_byte_length
          in
          let s, get_unicode_char =
            match source with
              | `ISO_8859_1 -> (s, fun s pos -> Uchar.of_char s.[pos])
              | `UTF_8 ->
                  ( s,
                    fun s pos ->
                      Uchar.utf_decode_uchar (String.get_utf_8_uchar s pos) )
              | `UTF_16BE ->
                  ( s,
                    fun s pos ->
                      Uchar.utf_decode_uchar (String.get_utf_16be_uchar s pos)
                  )
              | `UTF_16LE ->
                  ( s,
                    fun s pos ->
                      Uchar.utf_decode_uchar (String.get_utf_16le_uchar s pos)
                  )
              | `UTF_16 ->
                  let default =
                    ( "",
                      fun s pos ->
                        Uchar.utf_decode_uchar (String.get_utf_16be_uchar s pos)
                    )
                  in
                  if len < 2 then default
                  else (
                    let rem = String.sub s 2 (len - 2) in
                    match (s.[0], s.[1]) with
                      | '\xfe', '\xff' ->
                          ( rem,
                            fun s pos ->
                              Uchar.utf_decode_uchar
                                (String.get_utf_16be_uchar s pos) )
                      | '\xff', '\xfe' ->
                          ( rem,
                            fun s pos ->
                              Uchar.utf_decode_uchar
                                (String.get_utf_16le_uchar s pos) )
                      | _ -> default)
          in
          if target = `UTF_16 then add_unicode_char buf Uchar.bom;
          let len = String.length s in
          let rec f pos =
            if pos = len then Buffer.contents buf
            else (
              let c = get_unicode_char s pos in
              add_unicode_char buf c;
              f (pos + unicode_byte_length c))
          in
          f 0
end
