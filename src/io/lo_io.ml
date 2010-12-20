open Dtools

module S = LO.Server

let log = Log.make ["osc"]

let conf_oss =
  Conf.void ~p:(Configure.conf#plug "oss")
    "Interactions through the OSC protocol."

let conf_port =
  Conf.int ~p:(conf_oss#plug "port") ~d:7777
    "Port for OSC server."

let osc_bool = ref []
let osc_float = ref []

let handler path data =
  List.iter
    (fun d ->
      try
        match d with
          | `Float f | `Double f ->
            log#f 6 "Float %f on path %s" f path;
            let v = List.assoc path !osc_float in
            v := f
          | `True | `False ->
            let b = (d = `True) in
            let v = List.assoc path !osc_bool in
            v := b
          | _ -> ()
      with
        | _ -> ()
    ) data

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
      osc_float := (path,v) :: !osc_float;
      start_server ();
      Lang.val_fun [] ~ret_t:Lang.float_t (fun p _ -> Lang.float !v))

let () =
  Lang.add_builtin "osc.bool" ~category:"Interaction"
    ["",Lang.string_t,None,None; "",Lang.bool_t,None,None]
    (Lang.fun_t [] Lang.bool_t)
    ~descr:"Read a boolean from an OSC path."
    (fun p _ ->
      let path = Lang.to_string (Lang.assoc "" 1 p) in
      let v = Lang.to_bool (Lang.assoc "" 2 p) in
      let v = ref v in
      osc_bool := (path,v) :: !osc_bool;
      start_server ();
      Lang.val_fun [] ~ret_t:Lang.bool_t (fun p _ -> Lang.bool !v))
