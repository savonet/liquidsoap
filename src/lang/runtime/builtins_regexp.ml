(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

open Lang_regexp

let _ =
  let meth =
    [
      ("test", ([], test_t), "Match a string with the expressionn.", test_fun);
      ( "split",
        ([], split_t),
        "Split a string on the given regular expression.",
        split_fun );
      ( "exec",
        ([], exec_t),
        "Extract substrings from a string. Returns a list of (index,value). If \
         the list does not have a pair associated to some index, it means that \
         the corresponding pattern was not found.",
        exec_fun );
      ( "replace",
        ([], replace_t),
        "Replace substrings matched by the regexp by another string returned \
         by a function.",
        replace_fun );
    ]
  in
  let t =
    Lang_core.method_t RegExp.t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  Lang_core.add_builtin "regexp" ~category:`String
    ~descr:"Create a regular expression"
    [
      ( "flags",
        Lang_core.list_t Lang_core.string_t,
        Some (Lang_core.list []),
        Some
          (Printf.sprintf "List of flags. Valid flags: %s."
             (String.concat ", "
                (List.map
                   (fun f ->
                     Printf.sprintf "`\"%s\"`" (string_of_regexp_flag f))
                   all_regexp_flags))) );
      ("", Lang_core.string_t, None, None);
    ]
    t
    (fun p ->
      let flags =
        List.map
          (fun v ->
            try regexp_flag_of_string (Lang_core.to_string v)
            with _ ->
              raise (Error.Invalid_value (v, "Invalid regexp flag", [])))
          (Lang_core.to_list (List.assoc "flags" p))
      in
      let descr = Lang_core.to_string (List.assoc "" p) in
      let regexp =
        let flags =
          List.fold_left
            (fun l f ->
              match f with
                | `i -> `CASELESS :: l
                (* `g is handled at the call level. *)
                | `g -> l
                | `s -> `DOTALL :: l
                | `m -> `MULTILINE :: l)
            [] flags
        in
        match Re.Pcre.regexp ~flags descr with
          | v -> v
          | exception exn ->
              Runtime_error.raise
                ~message:
                  (Printf.sprintf "Error while creating regular expression: %s"
                     (Printexc.to_string exn))
                ~pos:(Lang_core.pos p) "string"
      in
      let v = RegExp.to_value { descr; flags; regexp } in
      let meth =
        List.map (fun (name, _, _, fn) -> (name, fn ~flags ~descr regexp)) meth
      in
      Lang_core.meth v meth)
