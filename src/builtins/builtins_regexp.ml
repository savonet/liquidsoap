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

type regexp = {
  descr : string;
  flags : [ `i | `g | `s | `m ] list;
  regexp : Pcre.regexp;
}

let all_regexp_flags = [`i; `g; `s; `m]

let cflags_of_flags (flags : [ `i | `g | `s | `m ] list) =
  List.fold_left
    (fun l f ->
      match f with
        | `i -> `CASELESS :: l
        (* `g is handled at the call level. *)
        | `g -> l
        | `s -> `DOTALL :: l
        | `m -> `MULTILINE :: l)
    [] flags

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

module RegExp = Value.MkAbstract (struct
  type content = regexp

  let name = "regexp"

  let escape_regex_descr =
    let escape_regex_formatter =
      Utils.escape
        ~special_char:(fun s pos len ->
          if List.mem s.[pos] ['\''; '/'] && len = 1 then true
          else Utils.utf8_special_char s pos len)
        ~escape_char:(fun s pos len ->
          if s.[pos] = '/' && len = 1 then "\\/"
          else Utils.escape_utf8_char s pos len)
        ~next:Utils.utf8_next
    in
    Utils.escape_string escape_regex_formatter

  let descr { descr; flags } =
    Printf.sprintf "r/%s/%s" (escape_regex_descr descr)
      (String.concat ""
         (List.sort Stdlib.compare (List.map string_of_regexp_flag flags)))

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Regexp cannot be represented as json"
      "json"

  let compare r r' =
    Stdlib.compare
      (r.descr, List.sort Stdlib.compare r.flags)
      (r'.descr, List.sort Stdlib.compare r'.flags)
end)

let test_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.bool_t

let test_fun ~flags:_ rex =
  Lang.val_fun
    [("", "", None)]
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.bool (Pcre.pmatch ~rex string))

let split_t =
  Lang.fun_t [(false, "", Lang.string_t)] (Lang.list_t Lang.string_t)

let split_fun ~flags:_ rex =
  Lang.val_fun
    [("", "", None)]
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      Lang.list (List.map Lang.string (Pcre.split ~rex string)))

let exec_t =
  Lang.fun_t
    [(false, "", Lang.string_t)]
    (Lang.list_t (Lang.product_t Lang.int_t Lang.string_t))

let exec_fun ~flags:_ regexp =
  Lang.val_fun
    [("", "", None)]
    (fun p ->
      let string = Lang.to_string (List.assoc "" p) in
      try
        let sub = Pcre.exec ~rex:regexp string in
        let n = Pcre.num_of_subs sub in
        let rec extract acc i =
          if i < n then (
            try extract ((i, Pcre.get_substring sub i) :: acc) (i + 1)
            with Not_found -> extract acc (i + 1))
          else List.rev acc
        in
        let l = extract [] 1 in
        Lang.list
          (List.map (fun (x, y) -> Lang.product (Lang.int x) (Lang.string y)) l)
      with Not_found -> Lang.list [])

let replace_t =
  Lang.fun_t
    [
      (false, "", Lang.fun_t [(false, "", Lang.string_t)] Lang.string_t);
      (false, "", Lang.string_t);
    ]
    Lang.string_t

let replace_fun ~flags regexp =
  Lang.val_fun
    [("", "", None); ("", "", None)]
    (fun p ->
      let subst = Lang.assoc "" 1 p in
      let subst s =
        let ret = Lang.apply subst [("", Lang.string s)] in
        Lang.to_string ret
      in
      let string = Lang.to_string (Lang.assoc "" 2 p) in
      let sub =
        if List.mem `g flags then Pcre.substitute else Pcre.substitute_first
      in
      let string =
        try sub ~rex:regexp ~subst string
        with Pcre.Error err ->
          Runtime_error.raise ~pos:(Lang.pos p)
            ~message:
              (Printf.sprintf "string.replace pcre error: %s"
                 (Utils.string_of_pcre_error err))
            "string"
      in
      Lang.string string)

let () =
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
        "Replace all substrings matched by a pattern by another string \
         returned by a function.",
        replace_fun );
    ]
  in
  let t =
    Lang.method_t RegExp.t
      (List.map (fun (name, typ, doc, _) -> (name, typ, doc)) meth)
  in
  Lang.add_builtin "regexp" ~category:`String
    ~descr:"Create a regular expression"
    [
      ( "flags",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some
          (Printf.sprintf "List of flags. Valid flags: %s."
             (String.concat ", "
                (List.map
                   (fun f ->
                     Printf.sprintf "`\"%s\"`" (string_of_regexp_flag f))
                   all_regexp_flags))) );
      ("", Lang.string_t, None, None);
    ]
    t
    (fun p ->
      let flags =
        List.map
          (fun v ->
            try regexp_flag_of_string (Lang.to_string v)
            with _ -> raise (Error.Invalid_value (v, "Invalid regexp flag")))
          (Lang.to_list (List.assoc "flags" p))
      in
      let iflags = Pcre.cflags (cflags_of_flags flags) in
      let descr = Lang.to_string (List.assoc "" p) in
      let regexp = Pcre.regexp ~iflags descr in
      let v = RegExp.to_value { descr; flags; regexp } in
      let meth =
        List.map (fun (name, _, _, fn) -> (name, fn ~flags regexp)) meth
      in
      Lang.meth v meth)
