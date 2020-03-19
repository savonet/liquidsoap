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
let get_clock = Tutils.lazy_cell (fun () -> new Clock.clock "OSS")

class output ~kind ~clock_safe ~on_start ~on_stop ~infallible ~start dev
  val_source =
  let channels = AFrame.channels_of_kind kind in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let name = Printf.sprintf "oss_out(%s)" dev in
  object (self)
    inherit
      Output.output
        ~infallible ~on_stop ~on_start ~content_kind:kind ~name
          ~output_kind:"output.oss" val_source start as super

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (get_clock () :> Clock.clock))

    val mutable fd = None

    method self_sync = fd <> None

    method open_device =
      let descr = Unix.openfile dev [Unix.O_WRONLY] 0o200 in
      fd <- Some descr;
      force set_format descr 16;
      force set_channels descr channels;
      force set_rate descr samples_per_second

    method close_device =
      match fd with
        | None -> ()
        | Some x ->
            Unix.close x;
            fd <- None

    method output_start = self#open_device

    method output_stop = self#close_device

    method output_reset =
      self#close_device;
      self#open_device

    method output_send memo =
      let fd = Utils.get_some fd in
      let buf = AFrame.content memo in
      let r = Audio.S16LE.size (Audio.channels buf) (Audio.length buf) in
      let s = Bytes.create r in
      Audio.S16LE.of_audio buf s 0;
      let w = Unix.write fd s 0 r in
      assert (w = r)
  end

class input ~kind ~clock_safe ~start ~on_stop ~on_start ~fallible dev =
  let channels = AFrame.channels_of_kind kind in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit
      Start_stop.input
        ~content_kind:kind ~source_kind:"oss"
        ~name:(Printf.sprintf "oss_in(%s)" dev)
        ~on_start ~on_stop ~fallible ~autostart:start as super

    method private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (get_clock () :> Clock.clock))

    val mutable fd = None

    method self_sync = fd <> None

    method private start = self#open_device

    method private open_device =
      let descr = Unix.openfile dev [Unix.O_RDONLY] 0o400 in
      fd <- Some descr;
      force set_format descr 16;
      force set_channels descr channels;
      force set_rate descr samples_per_second

    method private stop = self#close_device

    method private close_device =
      Unix.close (Utils.get_some fd);
      fd <- None

    method output_reset =
      self#close_device;
      self#open_device

    method input frame =
      assert (0 = AFrame.position frame);
      let fd = Utils.get_some fd in
      let buf = AFrame.content frame in
      let len = 2 * Array.length buf * Audio.Mono.length buf.(0) in
      let s = Bytes.create len in
      let r = Unix.read fd s 0 len in
      (* TODO: recursive read ? *)
      assert (len = r);
      Audio.S16LE.to_audio (Bytes.unsafe_to_string s) 0 buf;
      AFrame.add_break frame (AFrame.size ())
  end

let () =
  let k = Lang.kind_type_of_kind_format (Lang.any_with ~audio:1 ()) in
  Lang.add_operator "output.oss" ~active:true
    ( Output.proto
    @ [
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of the dedicated OSS clock." );
        ( "device",
          Lang.string_t,
          Some (Lang.string "/dev/dsp"),
          Some "OSS device to use." );
        ("", Lang.source_t k, None, None);
      ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Output
    ~descr:"Output the source's stream to an OSS output device."
    (fun p kind ->
      let e f v = f (List.assoc v p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let clock_safe = e Lang.to_bool "clock_safe" in
      let device = e Lang.to_string "device" in
      let source = List.assoc "" p in
      ( new output
          ~start ~on_start ~on_stop ~infallible ~kind ~clock_safe device source
        :> Source.source ));
  let k = Lang.kind_type_of_kind_format Lang.audio_any in
  Lang.add_operator "input.oss" ~active:true
    ( Start_stop.input_proto
    @ [
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of the dedicated OSS clock." );
        ( "device",
          Lang.string_t,
          Some (Lang.string "/dev/dsp"),
          Some "OSS device to use." );
      ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Input
    ~descr:"Stream from an OSS input device."
    (fun p kind ->
      let e f v = f (List.assoc v p) in
      let clock_safe = e Lang.to_bool "clock_safe" in
      let device = e Lang.to_string "device" in
      let start = Lang.to_bool (List.assoc "start" p) in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      ( new input ~kind ~start ~on_start ~on_stop ~fallible ~clock_safe device
        :> Source.source ))
