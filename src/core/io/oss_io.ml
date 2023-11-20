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

open Mm

external set_format : Unix.file_descr -> int -> int = "caml_oss_dsp_setfmt"
external set_channels : Unix.file_descr -> int -> int = "caml_oss_dsp_channels"
external set_rate : Unix.file_descr -> int -> int = "caml_oss_dsp_speed"

(** Wrapper for calling set_* functions and checking that the desired
  * value has been accepted. If not, the current behavior is a bit
  * too violent. *)
let force f fd x =
  let x' = f fd x in
  if x <> x' then failwith "cannot obtain desired OSS settings"

(** Dedicated clock. *)
let get_clock = Tutils.lazy_cell (fun () -> Clock.clock "OSS")

class output ~clock_safe ~on_start ~on_stop ~infallible ~register_telnet ~start
  dev val_source =
  let samples_per_second = Lazy.force Frame.audio_rate in
  let name = Printf.sprintf "oss_out(%s)" dev in
  object (self)
    inherit
      Output.output
        ~infallible ~register_telnet ~on_stop ~on_start ~name
          ~output_kind:"output.oss" val_source start as super

    inherit! Source.no_seek

    method! private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify ~pos:self#pos self#clock
          (Clock.create_known (get_clock () :> Source.clock))

    val mutable fd = None
    method! self_sync = (`Dynamic, fd <> None)

    method open_device =
      let descr = Unix.openfile dev [Unix.O_WRONLY; Unix.O_CLOEXEC] 0o200 in
      fd <- Some descr;
      force set_format descr 16;
      force set_channels descr self#audio_channels;
      force set_rate descr samples_per_second

    method close_device =
      match fd with
        | None -> ()
        | Some x ->
            Unix.close x;
            fd <- None

    method start = self#open_device
    method stop = self#close_device

    method send_frame memo =
      let fd = Option.get fd in
      let buf = AFrame.pcm memo in
      let len = Audio.length buf in
      let r = Audio.S16LE.size (Audio.channels buf) len in
      let s = Bytes.create r in
      Audio.S16LE.of_audio buf 0 s 0 len;
      let w = Unix.write fd s 0 r in
      assert (w = r)
  end

class input ~clock_safe ~start ~on_stop ~on_start ~fallible dev =
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit
      Start_stop.active_source
        ~get_clock ~clock_safe
        ~name:(Printf.sprintf "oss_in(%s)" dev)
        ~on_start ~on_stop ~fallible ~autostart:start ()

    inherit Source.no_seek
    val mutable fd = None
    method self_sync = (`Dynamic, fd <> None)
    method abort_track = ()
    method remaining = -1
    method seek_source = (self :> Source.source)
    method private start = self#open_device

    method private open_device =
      let descr = Unix.openfile dev [Unix.O_RDONLY; Unix.O_CLOEXEC] 0o400 in
      fd <- Some descr;
      force set_format descr 16;
      force set_channels descr self#audio_channels;
      force set_rate descr samples_per_second

    method private stop = self#close_device

    method private close_device =
      Unix.close (Option.get fd);
      fd <- None

    method get_frame frame =
      assert (0 = AFrame.position frame);
      let fd = Option.get fd in
      let buf = AFrame.pcm frame in
      let len = 2 * Array.length buf * Audio.Mono.length buf.(0) in
      let s = Bytes.create len in
      let r = Unix.read fd s 0 len in
      (* TODO: recursive read ? *)
      assert (len = r);
      Audio.S16LE.to_audio (Bytes.unsafe_to_string s) 0 buf 0 len;
      AFrame.add_break frame (AFrame.size ())
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  ignore
    (Lang.add_operator ~base:Modules.output "oss"
       (Output.proto
       @ [
           ( "clock_safe",
             Lang.bool_t,
             Some (Lang.bool true),
             Some "Force the use of the dedicated OSS clock." );
           ( "device",
             Lang.string_t,
             Some (Lang.string "/dev/dsp"),
             Some "OSS device to use." );
           ("", Lang.source_t frame_t, None, None);
         ])
       ~return_t:frame_t ~category:`Output ~meth:Output.meth
       ~descr:"Output the source's stream to an OSS output device."
       (fun p ->
         let e f v = f (List.assoc v p) in
         let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
         let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
         let start = Lang.to_bool (List.assoc "start" p) in
         let on_start =
           let f = List.assoc "on_start" p in
           fun () -> ignore (Lang.apply f [])
         in
         let on_stop =
           let f = List.assoc "on_stop" p in
           fun () -> ignore (Lang.apply f [])
         in
         let clock_safe = e Lang.to_bool "clock_safe" in
         let device = e Lang.to_string "device" in
         let source = List.assoc "" p in
         (new output
            ~start ~on_start ~on_stop ~infallible ~register_telnet ~clock_safe
            device source
           :> Output.output)));

  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "oss"
    (Start_stop.active_source_proto ~clock_safe:true ~fallible_opt:(`Yep false)
    @ [
        ( "device",
          Lang.string_t,
          Some (Lang.string "/dev/dsp"),
          Some "OSS device to use." );
      ])
    ~meth:(Start_stop.meth ()) ~return_t ~category:`Input
    ~descr:"Stream from an OSS input device."
    (fun p ->
      let e f v = f (List.assoc v p) in
      let clock_safe = e Lang.to_bool "clock_safe" in
      let device = e Lang.to_string "device" in
      let start = Lang.to_bool (List.assoc "start" p) in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      new input ~start ~on_start ~on_stop ~fallible ~clock_safe device)
