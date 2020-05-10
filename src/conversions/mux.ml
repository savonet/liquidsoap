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

(* TODO: restore with proper kind unification... *)
(*
(** Muxing takes a main and an auxiliary source.
  * The auxiliary source streams only one kind of content,
  * the main has no channel of that kind, anything for the others. *)

module Muxer = struct
  module Generator = Generator.From_audio_video

  (* The kind of value shared by a producer and a consumer. *)
  type control = {
    lock : Mutex.t;
    generator : Generator.t;
    mutable buffering : bool;
    mutable main_abort : bool;
    mutable aux_abort : bool;
  }

  let proceed control f = Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~kind ~name c =
    object (self)
      inherit Source.source kind ~name

      method self_sync = false

      method stype = Source.Fallible

      method remaining = proceed c (fun () -> Generator.remaining c.generator)

      method is_ready = proceed c (fun () -> not c.buffering)

      method private get_frame frame =
        proceed c (fun () ->
            Generator.fill c.generator frame;
            if Frame.is_partial frame && Generator.length c.generator = 0 then (
              self#log#important "Buffer emptied, start buffering...";
              c.buffering <- true ))

      method abort_track =
        proceed c (fun () ->
            c.main_abort <- true;
            c.aux_abort <- true)
    end

  class consumer ~producer ~mode ~content ~max_buffer ~pre_buffer ~kind
    source_val c =
    let prebuf = Frame.master_of_seconds pre_buffer in
    let max_buffer = Frame.master_of_seconds max_buffer in
    let autostart = true in
    let output_kind =
      match (mode, content) with
        | `Main, `Audio -> "audio_main"
        | `Main, `Video -> "video_main"
        | `Aux, `Audio -> "audio_aux"
        | `Aux, `Video -> "video_aux"
        | _ -> assert false
    in
    object (self)
      inherit
        Output.output
          ~output_kind ~content_kind:kind ~infallible:false
          ~on_start:(fun () -> ())
          ~on_stop:(fun () -> ())
          source_val autostart

      val source = Lang.to_source source_val

      method output_reset = ()

      method output_start = ()

      method output_stop = ()

      method private set_clock =
        Clock.unify self#clock producer#clock;
        Clock.unify self#clock source#clock

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

            Generator.feed_from_frame ~mode:content c.generator frame;
            if Generator.length c.generator > prebuf then (
              c.buffering <- false;
              if Generator.buffered_length c.generator > max_buffer then
                Generator.remove c.generator
                  (Generator.length c.generator - max_buffer) ))
    end

  let create ~name ~pre_buffer ~max_buffer ~kind ~main_kind ~main_source
      ~main_content ~aux_source ~aux_kind ~aux_content () =
    let lock = Mutex.create () in
    let control =
      {
        generator = Generator.create `Both;
        lock;
        buffering = true;
        main_abort = false;
        aux_abort = false;
      }
    in
    let producer = new producer ~name ~kind control in
    ignore
      (new consumer
         ~producer ~mode:`Main ~kind:main_kind ~content:main_content main_source
         ~max_buffer ~pre_buffer control);
    ignore
      (new consumer
         ~producer ~mode:`Aux ~kind:aux_kind ~content:aux_content aux_source
         ~max_buffer ~pre_buffer control);
    producer
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
    (fun p ->
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      let main_source = List.assoc "" p in
      let main_content = `Audio in
      let main_kind = Frame.{ kind with video = Zero; midi = Zero } in
      let aux_source = List.assoc "video" p in
      let aux_content = `Video in
      let aux_kind = Frame.{ kind with audio = Zero; midi = Zero } in
      Muxer.create ~name:"mux_video" ~pre_buffer ~max_buffer ~kind:Lang.any ~main_source
        ~main_content ~main_kind ~aux_source ~aux_content ~aux_kind ())

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
    (fun p ->
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      let main_source = List.assoc "" p in
      let main_content = `Video in
      let main_kind = Frame.{ kind with audio = Zero; midi = Zero } in
      let aux_source = List.assoc "audio" p in
      let aux_content = `Audio in
      let aux_kind = Frame.{ kind with video = Zero; midi = Zero } in
      Muxer.create ~name:"mux_audio" ~pre_buffer ~max_buffer ~kind:Lang.any ~main_source
        ~main_content ~main_kind ~aux_source ~aux_content ~aux_kind ())
*)
