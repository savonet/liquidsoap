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

(** Muxing takes a main and an auxiliary source.
  * The auxiliary source streams only one kind of content,
  * the main has no channel of that kind, anything for the others. *)

module Muxer = struct
  module Generator = Generator.From_audio_video_plus

  (* The kind of value shared by a producer and a consumer. *)
  type control = {
    lock : Mutex.t;
    generator : Generator.t;
    mutable main_buffering : bool;
    mutable aux_buffering : bool;
    mutable main_abort : bool;
    mutable aux_abort : bool;
  }

  let proceed control f = Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~kind ~log_ref ~name c =
    object (self)
      inherit Source.source kind ~name

      initializer log_ref := self#log#important "%s"

      method self_sync = false

      method stype = Source.Fallible

      method remaining = proceed c (fun () -> Generator.remaining c.generator)

      method is_ready =
        proceed c (fun () -> not (c.main_buffering || c.aux_buffering))

      method private get_frame frame =
        proceed c (fun () ->
            Generator.fill c.generator frame;
            if Frame.is_partial frame && Generator.length c.generator = 0 then (
              self#log#important "Buffer emptied, start buffering...";
              c.main_buffering <- true;
              c.aux_buffering <- true ))

      method abort_track =
        proceed c (fun () ->
            c.main_abort <- true;
            c.aux_abort <- true)
    end

  class consumer ~mode ~content ~pre_buffer ~max_buffer ~kind source_val c =
    let prebuf = Frame.master_of_seconds pre_buffer in
    let maxbuf = Frame.master_of_seconds max_buffer in
    let autostart = true in
    object (self)
      inherit
        Output.output
          ~output_kind:"buffer" ~content_kind:kind ~infallible:false
          ~on_start:(fun () -> ())
          ~on_stop:(fun () -> ())
          source_val autostart

      method output_reset = ()

      method output_start = ()

      method output_stop = ()

      val source = Lang.to_source source_val

      method private feed_from_frame gen frame =
        let pts = Frame.pts frame in
        match content with
          | `Audio ->
              Generator.put_audio ~pts gen (AFrame.content frame) 0
                (AFrame.position frame)
          | `Video ->
              Generator.put_video ~pts gen (VFrame.content frame) 0
                (VFrame.position frame)

      method output_send frame =
        proceed c (fun () ->
            ( match mode with
              | `Main ->
                  if c.main_abort then (
                    c.main_abort <- false;
                    source#abort_track )
              | `Aux ->
                  if c.aux_abort then (
                    c.aux_abort <- false;
                    source#abort_track ) );

            self#feed_from_frame c.generator frame;

            if Generator.length c.generator > prebuf then (
              match mode with
                | `Main -> c.main_buffering <- false
                | `Aux -> c.aux_buffering <- false );

            if Generator.length c.generator > maxbuf then
              Generator.remove c.generator
                (Generator.length c.generator - maxbuf))
    end

  let create ~name ~log_overfull ~pre_buffer ~max_buffer ~kind ~main_kind
      ~main_source ~main_content ~aux_source ~aux_kind ~aux_content () =
    let max_ticks = Frame.master_of_seconds max_buffer in
    let log_ref = ref (fun _ -> ()) in
    let log x = !log_ref x in
    let lock = Mutex.create () in
    let control =
      {
        generator =
          Generator.create ~lock ~kind ~log ~log_overfull
            ~overfull:(`Drop_old max_ticks) `Both;
        lock;
        main_buffering = true;
        aux_buffering = true;
        main_abort = false;
        aux_abort = false;
      }
    in
    ignore
      (new consumer
         ~mode:`Main ~kind:main_kind ~content:main_content main_source
         ~pre_buffer ~max_buffer control);
    ignore
      (new consumer
         ~mode:`Aux ~kind:aux_kind ~content:aux_content aux_source ~pre_buffer
         ~max_buffer control);
    new producer ~name ~log_ref ~kind control
end

let base_proto =
  [
    ( "buffer",
      Lang.float_t,
      Some (Lang.float 1.),
      Some "Amount of data to pre-buffer, in seconds." );
    ( "max",
      Lang.float_t,
      Some (Lang.float 10.),
      Some "Maximum amount of buffered data, in seconds." );
    ( "log_overfull",
      Lang.bool_t,
      Some (Lang.bool true),
      Some "Log when the source's buffer is overfull." );
  ]

let () =
  let out_t = Lang.kind_type_of_kind_format Lang.any in
  let { Frame.audio; video; midi } = Lang.of_frame_kind_t out_t in
  let main_t = Lang.frame_kind_t ~audio ~video:Lang.zero_t ~midi in
  let aux_t = Lang.frame_kind_t ~audio:Lang.zero_t ~video ~midi:Lang.zero_t in
  Lang.add_operator "mux_video" ~category:Lang.Conversions
    ~descr:
      "Add video channnels to a stream. Track marks and metadata are taken \
       from both sources."
    ~return_t:out_t
    ( base_proto
    @ [
        ("video", Lang.source_t aux_t, None, None);
        ("", Lang.source_t main_t, None, None);
      ] )
    (fun p kind ->
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let main_source = List.assoc "" p in
      let main_content = `Audio in
      let main_kind = Lang.frame_kind_of_kind_type main_t in
      let aux_source = List.assoc "video" p in
      let aux_content = `Video in
      let aux_kind = Lang.frame_kind_of_kind_type aux_t in
      Muxer.create ~name:"mux_video" ~pre_buffer ~max_buffer ~kind ~log_overfull
        ~main_source ~main_content ~main_kind ~aux_source ~aux_content ~aux_kind
        ())

let () =
  let out_t = Lang.kind_type_of_kind_format Lang.any in
  let { Frame.audio; video; midi } = Lang.of_frame_kind_t out_t in
  let main_t = Lang.frame_kind_t ~audio:Lang.zero_t ~video ~midi in
  let aux_t = Lang.frame_kind_t ~audio ~video:Lang.zero_t ~midi:Lang.zero_t in
  Lang.add_operator "mux_audio" ~category:Lang.Conversions
    ~descr:
      "Mux an audio stream into an audio-free stream. Track marks and metadata \
       are taken from both sources."
    ~return_t:out_t
    ( base_proto
    @ [
        ("audio", Lang.source_t aux_t, None, None);
        ("", Lang.source_t main_t, None, None);
      ] )
    (fun p kind ->
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      let log_overfull = Lang.to_bool (List.assoc "log_overfull" p) in
      let main_source = List.assoc "" p in
      let main_content = `Video in
      let main_kind = Lang.frame_kind_of_kind_type main_t in
      let aux_source = List.assoc "audio" p in
      let aux_content = `Audio in
      let aux_kind = Lang.frame_kind_of_kind_type aux_t in
      Muxer.create ~name:"mux_audio" ~pre_buffer ~max_buffer ~kind ~log_overfull
        ~main_source ~main_content ~main_kind ~aux_source ~aux_content ~aux_kind
        ())
