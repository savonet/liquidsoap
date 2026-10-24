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

let input_option_names =
  Lazy.Mutexed.from_fun (fun () ->
      List.map
        (fun (o : Avutil.Options.opt) -> o.name)
        (Avutil.Options.opts Av.container_options))

(* io.md §1.1: int, then float, then string, a later name replacing an
   earlier one. *)
let option_table p =
  let table = Hashtbl.create 10 in
  let add arg convert =
    let v = List.assoc arg p in
    List.iter
      (fun pair ->
        let name, value = Lang.to_product pair in
        let name = Lang.to_string name in
        if not (List.mem name (Lazy.Mutexed.force input_option_names)) then
          Lang.raise_error
            ~pos:(Option.to_list (Value.pos v))
            ~message:(Printf.sprintf "Unknown FFmpeg input option: %s" name)
            "ffmpeg";
        Hashtbl.replace table name (convert value))
      (Lang.to_list v)
  in
  add "int_args" (fun v -> `Int (Lang.to_int v));
  add "float_args" (fun v -> `Float (Lang.to_float v));
  add "string_args" (fun v -> `String (Lang.to_string v));
  table

let input_format p =
  match Lang.to_valued_option Lang.to_string (List.assoc "format" p) with
    | None -> None
    | Some name -> (
        match Av.Format.find_input_format name with
          | Some format -> Some format
          | None ->
              Lang.raise_error
                ~pos:(Option.to_list (Value.pos (List.assoc "format" p)))
                ~message:"Could not find ffmpeg input format with that name"
                "ffmpeg")

let metadata_filter p =
  Option.map
    (fun filter m ->
      Frame.Metadata.from_list
        (Lang.to_metadata_list
           (Lang.apply filter
              [("", Lang.metadata_list (Frame.Metadata.to_list m))])))
    (Lang.to_option (List.assoc "metadata_filter" p))

let proto ~self_sync =
  Start_stop.active_source_proto ~fallible_opt:`Nope
  @ [
      ( "int_args",
        Lang.list_t (Lang.product_t Lang.string_t Lang.int_t),
        Some (Lang.list []),
        Some "FFmpeg options, each stored as an integer option." );
      ( "float_args",
        Lang.list_t (Lang.product_t Lang.string_t Lang.float_t),
        Some (Lang.list []),
        Some "FFmpeg options, each stored as a float option." );
      ( "string_args",
        Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),
        Some (Lang.list []),
        Some "FFmpeg options, each stored as a string option." );
      ( "metadata_filter",
        Lang.nullable_t
          (Lang.fun_t [(false, "", Lang.metadata_t)] Lang.metadata_t),
        Some Lang.null,
        Some
          "Metadata filter, replacing the default one, which drops the keys \
           starting with `id3v2_priv`." );
      ( "deduplicate_metadata",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Drop metadata equal to the previous one of the same connection."
      );
      ( "new_track_on_metadata",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Start a new track at each metadata." );
      ( "max_buffer",
        Lang.float_t,
        Some (Lang.float 5.),
        Some "Maximum decoded data held by the source, in seconds." );
      self_sync;
      ( "debug",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Log every fault with its backtrace at error level." );
      ( "poll_delay",
        Lang.float_t,
        Some (Lang.float 2.),
        Some
          "Delay before a retry after a failure, and interval of the URL check \
           while failed, in seconds." );
      ( "format",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Input format short name." );
      ( "trim_url",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Strip leading and trailing whitespace from the URL." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 10.),
        Some
          "Longest a single blocking read may last, in seconds, on an input \
           that is not listening." );
      ("", Lang.getter_t Lang.string_t, None, Some "URL to read from.");
    ]

let meth () =
  Lang.
    [
      {
        name = "url";
        scheme = ([], fun_t [] string_t);
        descr = "Current URL.";
        value = (fun s -> val_fun [] (fun _ -> string s#url));
      };
      {
        name = "set_url";
        scheme = ([], fun_t [(false, "", getter_t string_t)] unit_t);
        descr = "Set the URL, used at the next connection.";
        value =
          (fun s ->
            val_fun
              [("", "", None)]
              (fun p ->
                s#set_url (to_string_getter (List.assoc "" p));
                unit));
      };
      {
        name = "status";
        scheme = ([], fun_t [] string_t);
        descr =
          "Status: `\"stopped\"`, `\"starting\"`, `\"polling\"`, `\"connected \
           <url>\"`, `\"stopping\"` or `\"failed\"`.";
        value = (fun s -> val_fun [] (fun _ -> string s#status));
      };
    ]

let callbacks ~connect_arg_t ~connect_value =
  Lang.
    [
      {
        name = "on_connect";
        params = [];
        descr = "when connected, before the source plays the connection's data.";
        register_deprecated_argument = true;
        arg_t = connect_arg_t;
        register =
          (fun ~params:_ s f ->
            s#register_on_connect (fun headers -> f (connect_value headers)));
      };
      {
        name = "on_disconnect";
        params = [];
        descr = "when a connection whose `on_connect` callbacks ran ends.";
        register_deprecated_argument = true;
        arg_t = [];
        register =
          (fun ~params:_ s f -> s#register_on_disconnect (fun () -> f []));
      };
      {
        name = "on_error";
        params = [];
        descr = "on each connection or feeding failure.";
        register_deprecated_argument = true;
        arg_t = [(false, "", error_t)];
        register =
          (fun ~params:_ s f ->
            s#register_on_error (fun e -> f [("", error e)]));
      };
    ]
  @ Start_stop.callbacks ~label:"source"

let create ~kind p =
  let options = option_table p in
  Input_ffmpeg.create ~kind:(kind options)
    ~url:(Lang.to_string_getter (List.assoc "" p))
    ~options ~format:(input_format p)
    ~poll_delay:(Lang.to_float (List.assoc "poll_delay" p))
    ~timeout:(Lang.to_float (List.assoc "timeout" p))
    ~max_buffer:(Lang.to_float (List.assoc "max_buffer" p))
    ~metadata_filter:(metadata_filter p)
    ~deduplicate_metadata:(Lang.to_bool (List.assoc "deduplicate_metadata" p))
    ~new_track_on_metadata:(Lang.to_bool (List.assoc "new_track_on_metadata" p))
    ~debug:(Lang.to_bool (List.assoc "debug" p))
    ~trim_url:(Lang.to_bool (List.assoc "trim_url" p))
    ~autostart:(Lang.to_bool (List.assoc "start" p))
    ()

let return_t () = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty

let self_sync_description =
  "This source paces its clock while connected when `self_sync` is true."

let _ =
  Lang.add_operator ~base:Modules.input "ffmpeg" ~return_t:(return_t ())
    ~category:(`Input `Active)
    ~meth:(meth () @ Start_stop.meth ())
    ~callbacks:(callbacks ~connect_arg_t:[] ~connect_value:(fun _ -> []))
    ~self_sync_description
    ~descr:"Read a stream from a URL or a device with FFmpeg."
    (proto
       ~self_sync:
         ( "self_sync",
           Lang.getter_t Lang.bool_t,
           Some (Lang.bool false),
           Some
             "Pace the clock while connected. For live URLs only: on an input \
              whose reads do not block (a file, a generator) the clock then \
              runs as fast as the reads return." ))
    (fun p ->
      create p ~kind:(fun _ ->
          `Ffmpeg (Lang.to_bool_getter (List.assoc "self_sync" p))))

let _ =
  Lang.add_operator ~base:Modules.input "http" ~return_t:(return_t ())
    ~category:(`Input `Active)
    ~meth:(meth () @ Start_stop.meth ())
    ~callbacks:
      (callbacks
         ~connect_arg_t:[(false, "", Lang.metadata_t)]
         ~connect_value:(fun headers -> [("", Lang.metadata_list headers)]))
    ~self_sync_description ~descr:"Read an HTTP stream with FFmpeg."
    (proto
       ~self_sync:
         ( "self_sync",
           Lang.getter_t (Lang.nullable_t Lang.bool_t),
           Some Lang.null,
           Some
             "Pace the clock while connected. `null`: the clock keeps its own \
              pace, with this source as its sync source when ICY headers were \
              received." )
    @ [
        ( "user_agent",
          Lang.string_t,
          Some (Lang.string Liq_http.user_agent),
          Some "User agent." );
      ])
    (fun p ->
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let user_agent = Lang.to_string (List.assoc "user_agent" p) in
      let self_sync = Lang.to_getter (List.assoc "self_sync" p) in
      create p ~kind:(fun options ->
          Hashtbl.replace options "icy" (`Int 1);
          Hashtbl.replace options "user_agent" (`String user_agent);
          Hashtbl.replace options "rw_timeout"
            (`Int64 (Int64.of_float (timeout *. 1_000_000.)));
          `Http
            {
              Input_ffmpeg.user_agent;
              http_self_sync =
                (fun () -> Lang.to_valued_option Lang.to_bool (self_sync ()));
              icy = false;
            }))
