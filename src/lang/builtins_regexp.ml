(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

type regexp = {
  descr : string;
  flags : [ `i | `g | `s | `m ] list;
  regexp : Regexp.t;
}

let all_regexp_flags = [`i; `g; `s; `m]

let string_of_regexp_flag = function
  | `i -> "i"
  | `g -> "g"
  | `s -> "s"
  | `m -> "m"

let regexp_flag_of_string = function
  | "i" -> `i
  | "g" -> `g
  | "s" -> `s
  | "m" -> `m
  | _ -> assert false

let escape_regex_descr =
  let escape_regex_formatter =
    Lang_string.escape
      ~special_char:(fun s pos len ->
        if List.mem s.[pos] ['\''; '/'] && len = 1 then true
        else Lang_string.utf8_special_char s pos len)
      ~escape_char:(fun s pos len ->
        if s.[pos] = '/' && len = 1 then "\\/"
        else Lang_string.escape_utf8_char s pos len)
      ~next:Lang_string.utf8_next
  in
  Lang_string.escape_string escape_regex_formatter

let string_of_regexp { descr; flags } =
  Printf.sprintf "r/%s/%s" (escape_regex_descr descr)
    (String.concat ""
       (List.sort Stdlib.compare (List.map string_of_regexp_flag flags)))

module RegExp = Value.MkAbstract (struct
  type content = regexp

  let name = "regexp"
  let descr = string_of_regexp

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Regexp cannot be represented as json"
      "json"

  let compare r r' =
    Stdlib.compare
      (r.descr, List.sort Stdlib.compare r.flags)
      (r'.descr, List.sort Stdlib.compare r'.flags)
end)

let test_t = Lang_core.fun_t [(false, "", Lang_core.string_t)] Lang_core.bool_t

let test_fun rex =
  Lang_core.val_fun
    [("", "", None)]
    (fun p ->
      let string = Lang_core.to_string (List.assoc "" p) in
      Lang_core.bool (Regexp.test rex string))

let split_t =
  Lang_core.fun_t
    [(false, "", Lang_core.string_t)]
    (Lang_core.list_t Lang_core.string_t)

let split_fun rex =
  Lang_core.val_fun
    [("", "", None)]
    (fun p ->
      let string = Lang_core.to_string (List.assoc "" p) in
      Lang_core.list (List.map Lang_core.string (Regexp.split rex string)))

let exec_t =
  let matches_t =
    Lang_core.list_t (Lang_core.product_t Lang_core.int_t Lang_core.string_t)
  in
  Lang_core.fun_t
    [(false, "", Lang_core.string_t)]
    (Lang_core.method_t matches_t
       [
         ( "groups",
           ( [],
             Lang_core.list_t
               (Lang_core.product_t Lang_core.string_t Lang_core.string_t) ),
           "Named captures" );
       ])

let exec_fun regexp =
  Lang_core.val_fun
    [("", "", None)]
    (fun p ->
      let string = Lang_core.to_string (List.assoc "" p) in
      try
        let { Regexp.matches; groups } = Regexp.exec regexp string in
        let matches =
          Lang_core.list
            (List.fold_left
               (fun matches (pos, value) ->
                 match value with
                   | None -> matches
                   | Some value ->
                       Lang_core.product (Lang_core.int pos)
                         (Lang_core.string value)
                       :: matches)
               []
               (List.mapi (fun pos v -> (pos, v)) matches))
        in
        Lang_core.meth matches
          [
            ( "groups",
              Lang_core.list
                (List.map
                   (fun (name, value) ->
                     Lang_core.product (Lang_core.string name)
                       (Lang_core.string value))
                   groups) );
          ]
      with
        | Not_found -> Lang_core.list []
        | exn ->
            Runtime_error.raise ~pos:(Lang_core.pos p)
              ~message:
                (Printf.sprintf "Error while executing regular exception: %s"
                   (Printexc.to_string exn))
              "string")

let replace_t =
  Lang_core.fun_t
    [
      ( false,
        "",
        Lang_core.fun_t [(false, "", Lang_core.string_t)] Lang_core.string_t );
      (false, "", Lang_core.string_t);
    ]
    Lang_core.string_t

let replace_fun regexp =
  Lang_core.val_fun
    [("", "", None); ("", "", None)]
    (fun p ->
      let subst = Lang_core.assoc "" 1 p in
      let subst s =
        let ret = Lang_core.apply subst [("", Lang_core.string s)] in
        Lang_core.to_string ret
      in
      let string = Lang_core.to_string (Lang_core.assoc "" 2 p) in
      let string =
        try Regexp.substitute regexp ~subst string
        with exn ->
          Runtime_error.raise
            ~message:
              (Printf.sprintf "Error ehile executing regular expression: %s"
                 (Printexc.to_string exn))
            ~pos:(Lang_core.pos p) "string"
      in
      Lang_core.string string)

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
            with _ -> raise (Error.Invalid_value (v, "Invalid regexp flag")))
          (Lang_core.to_list (List.assoc "flags" p))
      in
      let descr = Lang_core.to_string (List.assoc "" p) in
      let regexp = Regexp.regexp ~flags descr in
      let v = RegExp.to_value { descr; flags; regexp } in
      let meth = List.map (fun (name, _, _, fn) -> (name, fn regexp)) meth in
      Lang_core.meth v meth)
