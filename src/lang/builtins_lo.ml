(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Dtools

module S = LO.Server

let m = Mutex.create ()

let log = Log.make ["osc"]

let conf_oss =
  Conf.void ~p:(Configure.conf#plug "osc")
    "Interactions through the OSC protocol."

let conf_port =
  Conf.int ~p:(conf_oss#plug "port") ~d:7777
    "Port for OSC server."

let osc_bool = ref []
let osc_float = ref []
let osc_float_pair = ref []

let handler path data =
  Mutex.lock m;
  (
    try
      (
        match data with
          | [|`Float f|] | [|`Double f|] ->
            log#f 6 "Float %f on path %s" f path;
            let v = List.assoc path !osc_float in
            v := f
          | [|`Float x; `Float y|] | [|`Double x; `Double y|] ->
            log#f 6 "Float pair (%f,%f) on path %s" x y path;
            let v = List.assoc path !osc_float_pair in
            v := (x,y)
          | [|`True as b|] | [|`False as b|] ->
            let b = (b = `True) in
            let v = List.assoc path !osc_bool in
            v := b
          | _ -> ()
      )
    with
      | _ -> ()
  );
  Mutex.unlock m

let server = ref None

let start_server () =
  if !server = None then
    let port = conf_port#get in
    let s = S.create port handler in
    server := Some s;
    ignore (Thread.create (fun () -> while true do S.recv s done) ())

let () =
  Lang.add_builtin "osc.float" ~category:"Interaction"
    ["",Lang.string_t,None,None; "",Lang.float_t,None,None]
    (Lang.fun_t [] Lang.float_t)
    ~descr:"Read a float from an OSC path."
    (fun p _ ->
      let path = Lang.to_string (Lang.assoc "" 1 p) in
      let v = Lang.to_float (Lang.assoc "" 2 p) in
      let v = ref v in
      Mutex.lock m;
      osc_float := (path,v) :: !osc_float;
      Mutex.unlock m;
      start_server ();
      Lang.val_fun [] ~ret_t:Lang.float_t
        (fun p _ ->
          Mutex.lock m;
          let v = Lang.float !v in
          Mutex.unlock m;
          v
        )
    )

let () =
  let t = Lang.product_t Lang.float_t Lang.float_t in
  Lang.add_builtin "osc.float_pair" ~category:"Interaction"
    ["",Lang.string_t,None,None; "",t,None,None]
    (Lang.fun_t [] t)
    ~descr:"Read a float from an OSC path."
    (fun p _ ->
      let path = Lang.to_string (Lang.assoc "" 1 p) in
      let (v1,v2) = Lang.to_product (Lang.assoc "" 2 p) in
      let v = Lang.to_float v1, Lang.to_float v2 in
      let v = ref v in
      Mutex.lock m;
      osc_float_pair := (path,v) :: !osc_float_pair;
      Mutex.unlock m;
      start_server ();
      Lang.val_fun [] ~ret_t:t
        (fun p _ ->
          Mutex.lock m;
          let x, y = !v in
          let v = Lang.product (Lang.float x) (Lang.float y) in
          Mutex.unlock m;
          v
        )
    )

let () =
  Lang.add_builtin "osc.bool" ~category:"Interaction"
    ["",Lang.string_t,None,None; "",Lang.bool_t,None,None]
    (Lang.fun_t [] Lang.bool_t)
    ~descr:"Read a boolean from an OSC path."
    (fun p _ ->
      let path = Lang.to_string (Lang.assoc "" 1 p) in
      let v = Lang.to_bool (Lang.assoc "" 2 p) in
      let v = ref v in
      Mutex.lock m;
      osc_bool := (path,v) :: !osc_bool;
      Mutex.unlock m;
      start_server ();
      Lang.val_fun [] ~ret_t:Lang.bool_t
        (fun p _ ->
          Mutex.lock m;
          let v = Lang.bool !v in
          Mutex.unlock m;
          v
        )
    )
