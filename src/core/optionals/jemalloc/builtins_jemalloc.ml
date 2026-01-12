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

let jemalloc = Lang.add_module ~base:Modules.runtime "jemalloc"
let mallctl = Lang.add_module ~base:jemalloc "mallctl"

let _ =
  Lang.add_builtin ~base:jemalloc "epoch" ~category:`System
    ~descr:"Refresh the epoch counter for statistics" [] Lang.unit_t (fun _ ->
      Jemalloc.epoch ();
      Lang.unit)

let () =
  let register (name, typ, cmd, from_val, to_val) =
    let get_t =
      Lang.method_t (Lang.fun_t [] typ)
        [
          ( "set",
            ([], Lang.fun_t [(false, "", typ)] Lang.unit_t),
            "Set option value" );
        ]
    in
    ignore
      (Lang.add_builtin ~base:mallctl name ~category:`System
         ~descr:
           ("Get an option of type " ^ name
          ^ " using mallctl. Returned value has the same type as a reference.")
         [("", Lang.string_t, None, Some "Option name")]
         get_t
         (fun p ->
           let name = Lang.to_string (List.assoc "" p) in
           let cmd value =
             try to_val (cmd name value)
             with Jemalloc.Invalid_property name ->
               Runtime_error.raise ~pos:(Lang.pos p)
                 ~message:
                   (if value = None then "Invalid property: " ^ name
                    else "Failed to set value on property " ^ name)
                 "invalid"
           in
           let get = Lang.val_fun [] (fun _ -> cmd None) in
           Lang.meth get
             [
               ( "set",
                 Lang.val_fun
                   [("", "", None)]
                   (fun p ->
                     let value = from_val (List.assoc "" p) in
                     ignore (cmd (Some value));
                     Lang.unit) );
             ]))
  in
  register ("bool", Lang.bool_t, Jemalloc.mallctl_bool, Lang.to_bool, Lang.bool);
  register ("int", Lang.int_t, Jemalloc.mallctl_int, Lang.to_int, Lang.int);
  register
    ( "string",
      Lang.string_t,
      Jemalloc.mallctl_string,
      Lang.to_string,
      Lang.string );
  ignore
    (Lang.add_builtin ~base:mallctl "flag" ~category:`System
       ~descr:"Set an option flag"
       [("", Lang.string_t, None, Some "Flag name")]
       Lang.unit_t
       (fun p ->
         let name = Lang.to_string (List.assoc "" p) in
         (try Jemalloc.mallctl_unit name
          with Jemalloc.Invalid_property name ->
            Runtime_error.raise ~pos:(Lang.pos p)
              ~message:("Invalid property: " ^ name)
              "invalid");
         Lang.unit))

let _ =
  let stat_t =
    Lang.record_t
      [
        ("active", Lang.int_t);
        ("resident", Lang.int_t);
        ("allocated", Lang.int_t);
        ("mapped", Lang.int_t);
      ]
  in
  let stat () =
    let { Jemalloc.active; resident; allocated; mapped } =
      Jemalloc.get_memory_stats ()
    in
    Lang.record
      [
        ("active", Lang.int active);
        ("resident", Lang.int resident);
        ("allocated", Lang.int allocated);
        ("mapped", Lang.int mapped);
      ]
  in
  Lang.add_builtin ~base:jemalloc "memory_stats" ~category:`System
    ~descr:"Return memory allocation stats from `jemalloc`." [] stat_t (fun _ ->
      stat ())

let _ =
  Lang.add_builtin ~base:jemalloc "version" ~category:`System
    ~descr:"Jemalloc version information" []
    (Lang.method_t Lang.string_t
       [
         ("major", ([], Lang.int_t), "Major version");
         ("minor", ([], Lang.int_t), "Minor version");
         ("git_version", ([], Lang.string_t), "Git version");
       ])
    (fun _ ->
      let version, major, minor, git = Jemalloc.version () in
      Lang.meth (Lang.string version)
        [
          ("major", Lang.int major);
          ("minor", Lang.int minor);
          ("git", Lang.string git);
        ])
