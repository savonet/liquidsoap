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
  let rec f ~name ~descr ~final_key cur path =
    match path with
      | el :: rem ->
          let cur =
            try cur#path [el]
            with Dtools.Conf.Unbound _ ->
              let name, descr, key =
                if rem = [] then (name, descr, final_key)
                else (String.capitalize_ascii el, [], Dtools.Conf.void)
              in
              key ~p:(cur#plug el) name ~comments:descr
          in
          f ~name ~descr ~final_key cur rem
      | [] -> cur
  in
  let univ = Lang.univ_t ~constraints:[Lang_types.Dtools] () in
  add_builtin ~cat:Liq "register" ~descr:"Register a new setting."
    [
      ("name", Lang.string_t, None, Some "Settings name");
      ( "descr",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Settings description" );
      ( "on_change",
        Lang.fun_t [(false, "", univ)] Lang.unit_t,
        Some (Lang.val_cst_fun [("", univ, None)] Lang.unit),
        Some "Callback executed when the setting is changed." );
      ( "validate",
        Lang.fun_t [(false, "", univ)] Lang.bool_t,
        Some (Lang.val_cst_fun [("", univ, None)] (Lang.bool true)),
        Some "Callback executed to validate a new value." );
      ("", Lang.string_t, None, Some "Setting key");
      ("", univ, None, Some "Setting initial value");
    ]
    Lang.unit_t
    (fun p ->
      let name = Lang.to_string (List.assoc "name" p) in
      let descr = Lang.to_string (List.assoc "descr" p) in
      let descr = if descr = "" then [] else [descr] in
      let path = Lang.to_string (Lang.assoc "" 1 p) in
      let validate = Lang.to_fun ~t:Lang.bool_t (List.assoc "validate" p) in
      let on_change = Lang.to_fun ~t:Lang.unit_t (List.assoc "on_change" p) in
      let v = Lang.assoc "" 2 p in
      let make to_value b ?p ?l ?comments name =
        let conf = b ?p ?l ?comments name in
        conf#validate (fun v -> Lang.to_bool (validate [("", to_value v)]));
        conf#on_change (fun v -> ignore (on_change [("", to_value v)]));
        conf#ut
      in
      let final_key =
        match v.Lang.value with
          | Lang.String s -> make Lang.string (Dtools.Conf.string ~d:s)
          | Lang.Int s -> make Lang.int (Dtools.Conf.int ~d:s)
          | Lang.Bool s -> make Lang.bool (Dtools.Conf.bool ~d:s)
          | Lang.Float s -> make Lang.float (Dtools.Conf.float ~d:s)
          | Lang.List l ->
              let l = List.map Lang.to_string l in
              let to_value l =
                Lang.list ~t:Lang.string_t (List.map Lang.string l)
              in
              make to_value (Dtools.Conf.list ~d:l)
          | Lang.Tuple [] -> Dtools.Conf.void
          | _ -> assert false
      in
      ignore
        (f ~name ~descr ~final_key Configure.conf
           (Dtools.Conf.path_of_string path));
      Lang.unit)

let () =
  let set cast path v =
    (cast (Configure.conf#path (Dtools.Conf.path_of_string path)))#set v
  in
  add_builtin ~cat:Liq "set"
    ~descr:
      "Change some setting. Use `liquidsoap --conf-descr` and `liquidsoap \
       --conf-descr-key KEY` on the command-line to get some information about \
       available settings."
    [
      ("", Lang.string_t, None, None);
      ("", Lang.univ_t ~constraints:[Lang_types.Dtools] (), None, None);
    ]
    Lang.unit_t
    (fun p ->
      let path = Lang.assoc "" 1 p in
      let path_s = Lang.to_string path in
      let value = Lang.assoc "" 2 p in
      let get_error e =
        match e with
          | Dtools.Conf.Mismatch _ ->
              let t = Configure.conf#path (Dtools.Conf.path_of_string path_s) in
              let kind =
                match t#kind with
                  | Some "unit" -> "of type unit"
                  | Some "int" -> "of type int"
                  | Some "float" -> "of type float"
                  | Some "bool" -> "of type bool"
                  | Some "string" -> "of type string"
                  | Some "list" -> "of type [string]"
                  | _ -> assert false
              in
              let msg =
                Printf.sprintf "key %S is %s, thus cannot be set to %s" path_s
                  kind (Lang.print_value value)
              in
              Lang_errors.Invalid_value (value, msg)
          | Dtools.Conf.Invalid_Value _ ->
              let msg =
                Printf.sprintf "invalid value %s for key %S"
                  (Lang.print_value value) path_s
              in
              Lang_errors.Invalid_value (value, msg)
          | Dtools.Conf.Unbound (_, _) ->
              let msg =
                Printf.sprintf "there is no configuration key named %S!" path_s
              in
              Lang_errors.Invalid_value (path, msg)
          | _ -> e
      in
      try
        begin
          match value.Lang.value with
          | Lang.Tuple [] -> set Dtools.Conf.as_unit path_s ()
          | Lang.String s -> set Dtools.Conf.as_string path_s s
          | Lang.Int s -> set Dtools.Conf.as_int path_s s
          | Lang.Bool s -> set Dtools.Conf.as_bool path_s s
          | Lang.Float s -> set Dtools.Conf.as_float path_s s
          | Lang.List l ->
              let l = List.map Lang.to_string l in
              set Dtools.Conf.as_list path_s l
          | _ -> assert false
        end;
        Lang.unit
      with e ->
        let e = get_error e in
        if Tutils.has_started () then (
          let m =
            match e with
              | Lang_errors.Invalid_value (_, m) -> m
              | _ -> Printexc.to_string e
          in
          log#severe "WARNING: %s" m;
          Lang.unit )
        else raise e)

let () =
  let get cast path v =
    try (cast (Configure.conf#path (Dtools.Conf.path_of_string path)))#get
    with Dtools.Conf.Unbound (_, _) ->
      log#severe "WARNING: there is no configuration key named %S!" path;
      v
  in
  let univ = Lang.univ_t ~constraints:[Lang_types.Dtools] () in
  add_builtin "get" ~cat:Liq ~descr:"Get a setting's value."
    [("default", univ, None, None); ("", Lang.string_t, None, None)] univ
    (fun p ->
      let path = Lang.to_string (List.assoc "" p) in
      let v = List.assoc "default" p in
      match v.Lang.value with
        | Lang.Tuple [] -> Lang.unit
        | Lang.String s -> Lang.string (get Dtools.Conf.as_string path s)
        | Lang.Int s -> Lang.int (get Dtools.Conf.as_int path s)
        | Lang.Bool s -> Lang.bool (get Dtools.Conf.as_bool path s)
        | Lang.Float s -> Lang.float (get Dtools.Conf.as_float path s)
        | Lang.List l ->
            let l = List.map Lang.to_string l in
            Lang.list ~t:Lang.string_t
              (List.map Lang.string (get Dtools.Conf.as_list path l))
        | _ -> assert false)
