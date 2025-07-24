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

include Charset_base

let log = Log.make ["charset"]

let recode_string ~fail ~in_enc ~out_enc s =
  let max_string_length = conf_max_string_length#get in
  match String.length s with
    | l when max_string_length < l ->
        log#important
          "Trying to convert string over max length of %d bytes! Either \
           increase `settings.charset.max_string_length` or check how to \
           ignore this conversion."
          max_string_length;
        s
    | _ -> (
        try
          try C.recode_string ~in_enc ~out_enc s
          with e ->
            let in_enc =
              if in_enc == automatic_encoding () then
                Printf.sprintf "auto(%s)" (String.concat "," conf_encoding#get)
              else C.name_of in_enc
            in
            log#important "Failed to convert %S from %s to %s (%s)!" s in_enc
              (C.name_of out_enc) (Printexc.to_string e);
            s
        with
          | Unknown_encoding e when not fail ->
              log#important "Failed to convert %S: unknown encoding %s" s e;
              s
          | e when not fail ->
              log#important "Failed to convert %S: unknown error %s" s
                (Printexc.to_string e);
              s)

let convert ?(fail = false) ?source ?(target = C.utf8) =
  let in_enc =
    match source with Some e -> e | None -> automatic_encoding ()
  in
  recode_string ~fail ~in_enc ~out_enc:target
