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

open Mm

external set_format : Unix.file_descr -> int -> int = "caml_oss_dsp_setfmt"
external set_channels : Unix.file_descr -> int -> int = "caml_oss_dsp_channels"
external set_rate : Unix.file_descr -> int -> int = "caml_oss_dsp_speed"

module SyncSource = Clock.MkSyncSource (struct
  type t = unit

  let to_string _ = "oss"
end)

let sync_source = SyncSource.make ()

(** Wrapper for calling set_* functions and checking that the desired value has
    been accepted. If not, the current behavior is a bit too violent. *)
let force f fd x =
  let x' = f fd x in
  if x <> x' then failwith "cannot obtain desired OSS settings"

class output ~self_sync ~infallible ~register_telnet ~start dev val_source =
  let samples_per_second = Lazy.force Frame.audio_rate in
  let name = Printf.sprintf "oss_out(%s)" dev in
  object (self)
    inherit
      Output.output
        ~infallible ~register_telnet ~name ~output_kind:"output.oss" val_source
          start

    val mutable fd = None

    method self_sync =
      if self_sync then (`Dynamic, if fd <> None then Some sync_source else None)
      else (`Static, None)

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

class input ~self_sync ~start ~fallible dev =
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit
      Start_stop.active_source
        ~name:(Printf.sprintf "oss_in(%s)" dev)
        ~fallible ~autostart:start () as active_source

    val mutable fd = None

    method self_sync =
      if self_sync then (`Dynamic, if fd <> None then Some sync_source else None)
      else (`Static, None)

    method abort_track = ()
    method remaining = -1
    method seek_source = (self :> Source.source)
    method private start = self#open_device
    method private can_generate_frame = active_source#started

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

    method generate_frame =
      let length = Lazy.force Frame.size in
      let frame = Frame.create ~length self#content_type in
      let buf = Content.Audio.get_data (Frame.get frame Frame.Fields.audio) in
      let fd = Option.get fd in
      let len = 2 * Array.length buf * Audio.Mono.length buf.(0) in
      let s = Bytes.create len in
      let r = Unix.read fd s 0 len in
      (* TODO: recursive read ? *)
      assert (len = r);
      Audio.S16LE.to_audio (Bytes.unsafe_to_string s) 0 buf 0 len;
      Frame.set_data frame Frame.Fields.audio Content.Audio.lift_data buf
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
           ( "self_sync",
             Lang.bool_t,
             Some (Lang.bool true),
             Some "Mark the source as being synchronized by the OSS driver." );
           ( "device",
             Lang.string_t,
             Some (Lang.string "/dev/dsp"),
             Some "OSS device to use." );
           ("", Lang.source_t frame_t, None, None);
         ])
       ~return_t:frame_t ~category:`Output ~meth:Output.meth
       ~callbacks:Output.callbacks
       ~descr:"Output the source's stream to an OSS output device."
       (fun p ->
         let e f v = f (List.assoc v p) in
         let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
         let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
         let start = Lang.to_bool (List.assoc "start" p) in
         let self_sync = e Lang.to_bool "self_sync" in
         let device = e Lang.to_string "device" in
         let source = List.assoc "" p in
         (new output
            ~start ~infallible ~register_telnet ~self_sync device source
           :> Output.output)));

  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "oss"
    (Start_stop.active_source_proto ~fallible_opt:(`Yep false)
    @ [
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Mark the source as being synchronized by the OSS driver." );
        ( "device",
          Lang.string_t,
          Some (Lang.string "/dev/dsp"),
          Some "OSS device to use." );
      ])
    ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"source")
    ~return_t ~category:`Input ~descr:"Stream from an OSS input device."
    (fun p ->
      let e f v = f (List.assoc v p) in
      let self_sync = e Lang.to_bool "self_sync" in
      let device = e Lang.to_string "device" in
      let start = Lang.to_bool (List.assoc "start" p) in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      new input ~start ~fallible ~self_sync device)
