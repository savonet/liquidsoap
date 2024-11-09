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

let source = Muxer.source
let should_stop = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"builtin source shutdown" (fun () ->
      Atomic.set should_stop true)

let _ =
  Lang.add_builtin ~base:source "set_name" ~category:(`Source `Liquidsoap)
    ~descr:"Set the name of an operator."
    [
      ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.string_t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let s = Lang.assoc "" 1 p |> Lang.to_source in
      let n = Lang.assoc "" 2 p |> Lang.to_string in
      s#set_name n;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:source "last_metadata" ~category:(`Source `Liquidsoap)
    ~descr:"Return the last metadata from the source."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    (Lang.nullable_t Lang.metadata_t)
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      match s#last_metadata with None -> Lang.null | Some m -> Lang.metadata m)

let _ =
  Lang.add_builtin ~base:source "skip" ~category:(`Source `Liquidsoap)
    ~descr:"Skip to the next track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.unit_t
    (fun p ->
      (Lang.to_source (List.assoc "" p))#abort_track;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:source "seek" ~category:(`Source `Liquidsoap)
    ~descr:
      "Seek forward, in seconds. Returns the amount of time effectively seeked."
    [
      ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.float_t, None, None);
    ]
    Lang.float_t
    (fun p ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let time = Lang.to_float (Lang.assoc "" 2 p) in
      let len = Frame.main_of_seconds time in
      let ret = s#seek len in
      Lang.float (Frame.seconds_of_main ret))

let _ =
  Lang.add_builtin ~base:source "id" ~category:(`Source `Liquidsoap)
    ~descr:"Get the identifier of a source."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.string_t
    (fun p -> Lang.string (Lang.to_source (List.assoc "" p))#id)

let _ =
  Lang.add_builtin ~base:source "fallible" ~category:(`Source `Liquidsoap)
    ~descr:"Indicate if a source may fail, i.e. may not be ready to stream."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (Lang.to_source (List.assoc "" p))#fallible)

let _ =
  Lang.add_builtin ~base:source "is_ready" ~category:(`Source `Liquidsoap)
    ~descr:
      "Indicate if a source is ready to stream (we also say that it is \
       available), or currently streaming."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (Lang.to_source (List.assoc "" p))#is_ready)

let _ =
  Lang.add_builtin ~base:source "is_up" ~category:`System
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.bool_t ~descr:"Check whether a source is up."
    (fun p -> Lang.bool (Lang.to_source (Lang.assoc "" 1 p))#is_up)

let _ =
  Lang.add_builtin ~base:source "remaining" ~category:(`Source `Liquidsoap)
    ~descr:"Estimation of remaining time in the current track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let r = (Lang.to_source (List.assoc "" p))#remaining in
      let f = if r < 0 then infinity else Frame.seconds_of_main r in
      Lang.float f)

let _ =
  Lang.add_builtin ~base:source "elapsed" ~category:(`Source `Liquidsoap)
    ~descr:"Elapsed time in the current track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let d = (Lang.to_source (List.assoc "" p))#elapsed in
      let f = if d < 0 then infinity else Frame.seconds_of_main d in
      Lang.float f)

let _ =
  Lang.add_builtin ~base:source "duration" ~category:(`Source `Liquidsoap)
    ~descr:"Estimation of the duration in the current track."
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let d = (Lang.to_source (List.assoc "" p))#duration in
      let f = if d < 0 then infinity else Frame.seconds_of_main d in
      Lang.float f)

let _ =
  Lang.add_builtin ~category:(`Source `Liquidsoap) ~base:source "time"
    ~descr:"Get a source's time, based on its assigned clock"
    [("", Lang.source_t (Lang.univ_t ()), None, None)]
    Lang.float_t
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      let ticks = Clock.ticks s#clock in
      let frame_position = Lazy.force Frame.duration *. float ticks in
      Lang.float frame_position)

let _ =
  Lang.add_builtin ~base:source "on_shutdown" ~category:(`Source `Liquidsoap)
    [
      ("", Lang.source_t (Lang.univ_t ()), None, None);
      ("", Lang.fun_t [] Lang.unit_t, None, None);
    ]
    Lang.unit_t
    ~descr:
      "Register a function to be called when source is not used anymore by \
       another source."
    (fun p ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      let wrap_f () = ignore (Lang.apply f []) in
      s#on_sleep wrap_f;
      Lang.unit)

let flush_source ~log ~name ~ratio ~timeout ~sleep_latency s =
  let module Time = (val Clock.time_implementation () : Liq_time.T) in
  let open Time in
  let started = ref false in
  let stopped = ref false in
  let clock =
    Clock.create ~id:name ~sync:`Passive
      ~on_error:(fun exn bt ->
        stopped := true;
        Utils.log_exception ~log
          ~bt:(Printexc.raw_backtrace_to_string bt)
          (Printf.sprintf "Error while processing source: %s"
             (Printexc.to_string exn)))
      ()
  in
  let _ =
    new Output.dummy
      ~clock ~infallible:false
      ~on_start:(fun () -> ())
      ~on_stop:(fun () -> stopped := true)
      ~register_telnet:false ~autostart:true (Lang.source s)
  in
  Clock.start ~force:true clock;
  log#info "Start source streaming loop (ratio: %.02fx)" ratio;
  let start_time = Time.time () in
  let timeout = Time.of_float timeout in
  let timeout_time = Time.(start_time |+| timeout) in
  let sleep_latency = Time.of_float sleep_latency in
  let target_time () =
    Time.(start_time |+| sleep_latency |+| of_float (Clock.time clock /. ratio))
  in
  (try
     while (not (Atomic.get should_stop)) && not !stopped do
       if not !started then started := s#is_ready;
       if (not !started) && Time.(timeout_time |<=| start_time) then (
         log#important "Timeout while waiting for the source to start!";
         stopped := true)
       else (
         Clock.tick clock;
         let target_time = target_time () in
         if Time.(time () |<| (target_time |+| sleep_latency)) then
           sleep_until target_time)
     done
   with Clock.Has_stopped -> ());
  let processing_time = Time.(to_float (time () |-| start_time)) in
  let effective_ratio = Clock.time clock /. processing_time in
  log#info
    "Source processed. Total processing time: %.02fs, effective ratio: %.02fx"
    processing_time effective_ratio;
  Clock.stop clock

let _ =
  let log = Log.make ["source"; "dump"] in
  let kind = Lang.univ_t () in
  Lang.add_builtin ~base:source "dump" ~category:(`Source `Liquidsoap)
    ~descr:"Immediately encode the whole contents of a source into a file."
    ~flags:[`Experimental]
    [
      ("", Lang.format_t kind, None, Some "Encoding format.");
      ("", Lang.string_t, None, Some "Name of the file.");
      ("", Lang.source_t kind, None, Some "Source to encode.");
      ( "ratio",
        Lang.float_t,
        Some (Lang.float 50.),
        Some
          "Time ratio. A value of `50` means process data at `50x` real rate, \
           when possible." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 1.),
        Some
          "Stop processing the source if it has not started after the given \
           timeout." );
      ( "sleep_latency",
        Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "How much time ahead, in seconds, should we should be before pausing \
           the processing." );
    ]
    Lang.unit_t
    (fun p ->
      let proto =
        let p = Pipe_output.file_proto (Lang.univ_t ()) in
        List.filter_map (fun (l, _, v, _) -> Option.map (fun v -> (l, v)) v) p
      in
      let proto = ("fallible", Lang.bool true) :: proto in
      let p = (("id", Lang.string "source.drop") :: p) @ proto in
      let s = Pipe_output.new_file_output p in
      let ratio = Lang.to_float (List.assoc "ratio" p) in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let sleep_latency = Lang.to_float (List.assoc "sleep_latency" p) in
      flush_source ~log ~name:"source.dump" ~ratio ~timeout ~sleep_latency
        (s :> Source.source);
      log#info "Source dumped.";
      Lang.unit)

let _ =
  let log = Log.make ["source"; "drop"] in
  Lang.add_builtin ~base:source "drop" ~category:(`Source `Liquidsoap)
    ~descr:"Animate the source as fast as possible, dropping its output."
    ~flags:[`Experimental]
    [
      ("", Lang.source_t (Lang.univ_t ()), None, Some "Source to animate.");
      ( "ratio",
        Lang.float_t,
        Some (Lang.float 50.),
        Some
          "Time ratio. A value of `50` means process data at `50x` real rate, \
           when possible." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 1.),
        Some
          "Stop processing the source if it has not started after the given \
           timeout." );
      ( "sleep_latency",
        Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "How much time ahead, in seconds, should we should be before pausing \
           the processing." );
    ]
    Lang.unit_t
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      let ratio = Lang.to_float (List.assoc "ratio" p) in
      let timeout = Lang.to_float (List.assoc "timeout" p) in
      let sleep_latency = Lang.to_float (List.assoc "sleep_latency" p) in
      flush_source ~log ~name:"source.dump" ~ratio ~timeout ~sleep_latency s;
      Lang.unit)
