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

(** Create a buffer between two clocks.
  *
  * This creates an active operator in the inner clock (the action consists
  * in filling the buffer) but it does not create or force in any
  * way the clock that's going to animate it.
  *
  * We actually create two sources, to avoid the mess of having a same
  * source belonging to one clock but being animated by another.
  * This makes it possible to have the inner clock stop and shutdown
  * the source that feeds the buffer, without disturbing the other clock
  * in which the buffer-consumer will still behave OK (except obviously
  * that the buffer will empty). *)
module Buffer = struct
  module Generator = Generator.From_frames

  (* The kind of value shared by a producer and a consumer. *)
  type control = {
    lock: Mutex.t;
    generator: Generator.t;
    mutable buffering: bool;
    mutable abort: bool;
  }

  let proceed control f = Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~kind c =
    object (self)
      inherit Source.source kind ~name:"warp_prod"

      method self_sync = false

      method stype = Source.Fallible

      method remaining = proceed c (fun () -> Generator.remaining c.generator)

      method is_ready = proceed c (fun () -> not c.buffering)

      method private get_frame frame =
        proceed c (fun () ->
            assert (not c.buffering) ;
            Generator.fill c.generator frame ;
            if Frame.is_partial frame && Generator.length c.generator = 0 then (
              (self#log)#important "Buffer emptied, start buffering..." ;
              c.buffering <- true ))

      method abort_track = proceed c (fun () -> c.abort <- true)
    end

  class consumer ~autostart ~infallible ~on_start ~on_stop ~pre_buffer
    ~max_buffer ~kind source_val c =
    let prebuf = Frame.master_of_seconds pre_buffer in
    let maxbuf = Frame.master_of_seconds max_buffer in
    object
      inherit
        Output.output
          ~output_kind:"buffer" ~content_kind:kind ~infallible ~on_start
            ~on_stop source_val autostart

      method output_reset = ()

      method output_start = ()

      method output_stop = ()

      val source = Lang.to_source source_val

      method output_send frame =
        proceed c (fun () ->
            if c.abort then (
              c.abort <- false ;
              source#abort_track ) ;
            Generator.feed_from_frame c.generator frame ;
            if Generator.length c.generator > prebuf then (
              c.buffering <- false ;
              if Generator.length c.generator > maxbuf then
                Generator.remove c.generator
                  (Generator.length c.generator - maxbuf) ))
    end

  let create ~autostart ~infallible ~on_start ~on_stop ~pre_buffer ~max_buffer
      ~kind source_val =
    let control =
      {
        generator= Generator.create ();
        lock= Mutex.create ();
        buffering= true;
        abort= false;
      }
    in
    let _ =
      new consumer
        ~autostart ~infallible ~on_start ~on_stop ~kind source_val ~pre_buffer
        ~max_buffer control
    in
    new producer ~kind control
end

let () =
  let k = Lang.univ_t () in
  Lang.add_operator "buffer"
    ( Output.proto
    @ [ ( "buffer",
          Lang.float_t,
          Some (Lang.float 1.),
          Some "Amount of data to pre-buffer, in seconds." );
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum amount of buffered data, in seconds." );
        ("", Lang.source_t k, None, None) ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Liquidsoap
    ~descr:"Create a buffer between two different clocks."
    (fun p kind ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start = List.assoc "on_start" p in
      let on_stop = List.assoc "on_stop" p in
      let on_start () = ignore (Lang.apply ~t:Lang.unit_t on_start []) in
      let on_stop () = ignore (Lang.apply ~t:Lang.unit_t on_stop []) in
      let s = List.assoc "" p in
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      Buffer.create ~infallible ~autostart ~on_start ~on_stop ~pre_buffer
        ~max_buffer ~kind s)

module AdaptativeBuffer = struct
  module RB = Audio.Ringbuffer
  module MG = Generator.Metadata

  (* The kind of value shared by a producer and a consumer. *)
  (* TODO: also have breaks and metadata as in generators. *)
  type control = {
    lock: Mutex.t;
    rb: RB.t;
    mutable rb_length: float;
    (* average length of the ringbuffer in samples *)
    mg: MG.t;
    mutable buffering: bool;
    mutable abort: bool;
  }

  let proceed control f = Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~kind ~pre_buffer ~averaging ~limit c =
    let channels = (Frame.type_of_kind kind).Frame.audio in
    let prebuf = float (Frame.audio_of_seconds pre_buffer) in
    (* see get_frame for an explanation *)
    let alpha = log 2. *. AFrame.duration () /. averaging in
    object (self)
      inherit Source.source kind ~name:"buffer.adaptative_producer"

      method stype = Source.Fallible

      method self_sync = false

      method remaining = proceed c (fun () -> MG.remaining c.mg)

      method is_ready = proceed c (fun () -> not c.buffering)

      method private get_frame frame =
        proceed c (fun () ->
            assert (not c.buffering) ;
            (* Update the average length of the ringbuffer (with a damping
             coefficient in order not to be too sensitive to quick local
             variations). *)
            (* y: average length, dt: frame duration, x: read length, A=a/dt

             y(t+dt)=(1-a)y(t)+ax(t)
             y'(t)=(a/dt)(x(t)-y(t))
             y'(t)+Ay(t)=Ax(t)

             When x=x0 is constant and we start at y0, the solution is
             y(t) = (y0-x0)exp(-At)+x0

             half-life is at th = ln(2)/A
             we should thus choose alpha = (dt * ln 2)/th
          *)
            c.rb_length <-
              ((1. -. alpha) *. c.rb_length)
              +. (alpha *. float (RB.read_space c.rb)) ;
            (* Limit estimation *)
            c.rb_length <- min c.rb_length (prebuf *. limit) ;
            c.rb_length <- max c.rb_length (prebuf /. limit) ;
            (* Fill dlen samples of dst using slen samples of the ringbuffer. *)
            let fill dst dofs dlen slen =
              (* TODO: when the RB is low on space we'd better not fill the whole
               frame *)
              let slen = min slen (RB.read_space c.rb) in
              if slen > 0 then (
                let src = Audio.create channels slen in
                RB.read c.rb src ;
                if slen = dlen then
                  Audio.blit (Audio.sub src 0 slen) (Audio.sub dst dofs slen)
                else
                  (* TODO: we could do better than nearest interpolation. However,
                   for slight adaptations the difference should not really be
                   audible. *)
                  for c = 0 to channels - 1 do
                    let srcc = src.(c) in
                    let dstc = dst.(c) in
                    for i = 0 to dlen - 1 do
                      let x = srcc.{i * slen / dlen} in
                      dstc.{i + dofs} <- x
                    done
                  done )
            in
            (* We scale the reading so that the buffer always approximatively
             contains prebuf data. *)
            let scaling = c.rb_length /. prebuf in
            let scale n = int_of_float (float n *. scaling) in
            let unscale n = int_of_float (float n /. scaling) in
            let ofs = Frame.position frame in
            let len = Lazy.force Frame.size - ofs in
            let aofs = Frame.audio_of_master ofs in
            let alen = Frame.audio_of_master len in
            let buf = AFrame.content_of_type ~channels frame aofs in
            let salen = scale alen in
            fill buf aofs alen salen ;
            Frame.add_break frame (ofs + len) ;
            (* self#log#debug "filled %d from %d (x %f)" len ofs scaling; *)

            (* Fill in metadata *)
            let md = MG.metadata c.mg (scale len) in
            List.iter (fun (t, m) -> Frame.set_metadata frame (unscale t) m) md ;
            MG.advance c.mg
              (min (Frame.master_of_audio salen) (MG.length c.mg)) ;
            if Frame.is_partial frame then MG.drop_initial_break c.mg ;
            (* If there is no data left, we should buffer again. *)
            if RB.read_space c.rb = 0 then (
              (self#log)#important "Buffer emptied, start buffering..." ;
              (self#log)#debug "Current scaling factor is x%f." scaling ;
              MG.advance c.mg (MG.length c.mg) ;
              (* sync just in case *)
              c.buffering <- true ))

      method abort_track = proceed c (fun () -> c.abort <- true)
    end

  class consumer ~autostart ~infallible ~on_start ~on_stop ~pre_buffer ~reset
    ~kind source_val c =
    let channels = (Frame.type_of_kind kind).Frame.audio in
    let prebuf = Frame.audio_of_seconds pre_buffer in
    object
      inherit
        Output.output
          ~output_kind:"buffer" ~content_kind:kind ~infallible ~on_start
            ~on_stop source_val autostart

      method output_reset = ()

      method output_start = ()

      method output_stop = ()

      val source = Lang.to_source source_val

      method output_send frame =
        proceed c (fun () ->
            if c.abort then (
              c.abort <- false ;
              source#abort_track ) ;
            let len = AFrame.position frame in
            (* TODO: is this ok to start from 0? *)
            let buf = AFrame.content_of_type ~channels frame 0 in
            if RB.write_space c.rb < len then (
              (* Not enough write space, let's drop some data. *)
              let n = len - RB.write_space c.rb in
              RB.read_advance c.rb n ;
              MG.advance c.mg (Frame.master_of_audio n) ) ;
            RB.write c.rb (Audio.sub buf 0 len) ;
            MG.feed_from_frame c.mg frame ;
            if RB.read_space c.rb > prebuf then (
              c.buffering <- false ;
              if reset then
                c.rb_length <- float (Frame.audio_of_seconds pre_buffer) ))
    end

  let create ~autostart ~infallible ~on_start ~on_stop ~pre_buffer ~max_buffer
      ~averaging ~limit ~reset ~kind source_val =
    let channels = (Frame.type_of_kind kind).Frame.audio in
    let control =
      {
        lock= Mutex.create ();
        rb= RB.create channels (Frame.audio_of_seconds max_buffer);
        rb_length= float (Frame.audio_of_seconds pre_buffer);
        mg= MG.create ();
        buffering= true;
        abort= false;
      }
    in
    let _ =
      new consumer
        ~autostart ~infallible ~on_start ~on_stop ~kind source_val ~pre_buffer
        ~reset control
    in
    new producer ~kind ~pre_buffer ~averaging ~limit control
end

let () =
  let k = Lang.kind_type_of_kind_format Lang.audio_any in
  Lang.add_operator "buffer.adaptative"
    ( Output.proto
    @ [ ( "buffer",
          Lang.float_t,
          Some (Lang.float 1.),
          Some "Amount of data to pre-buffer, in seconds." );
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum amount of buffered data, in seconds." );
        ( "averaging",
          Lang.float_t,
          Some (Lang.float 30.),
          Some "Half-life for the averaging of the buffer size, in seconds." );
        ( "limit",
          Lang.float_t,
          Some (Lang.float 1.25),
          Some "Maximum acceleration or deceleration factor." );
        ( "reset",
          Lang.bool_t,
          Some (Lang.bool false),
          Some
            "Reset speed estimation to 1. when the source becomes available \
             again." );
        ("", Lang.source_t k, None, None) ] )
    ~kind:(Lang.Unconstrained k) ~category:Lang.Liquidsoap
    ~descr:
      "Create a buffer between two different clocks. The speed of the output \
       is adapted so that no buffer underrun or overrun occurs. This \
       wonderful behavior has a cost: the pitch of the sound might be changed \
       a little."
    ~flags:[Lang.Experimental]
    (fun p kind ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start = List.assoc "on_start" p in
      let on_stop = List.assoc "on_stop" p in
      let on_start () = ignore (Lang.apply ~t:Lang.unit_t on_start []) in
      let on_stop () = ignore (Lang.apply ~t:Lang.unit_t on_stop []) in
      let s = List.assoc "" p in
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let averaging = Lang.to_float (List.assoc "averaging" p) in
      let limit = Lang.to_float (List.assoc "limit" p) in
      let limit = if limit < 1. then 1. /. limit else limit in
      let reset = Lang.to_bool (List.assoc "reset" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      AdaptativeBuffer.create ~infallible ~autostart ~on_start ~on_stop
        ~pre_buffer ~max_buffer ~averaging ~limit ~reset ~kind s)
