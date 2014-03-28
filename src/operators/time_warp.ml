(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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
module Buffer =
struct

  module Generator = Generator.From_frames

  (* The kind of value shared by a producer and a consumer. *)
  type control = {
    lock : Mutex.t ;
    generator : Generator.t ;
    mutable buffering : bool ;
    mutable abort : bool
  }

  let proceed control f =
    Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~kind c =
  object (self)
    inherit Source.source kind ~name:"warp_prod"

    method stype = Source.Fallible

    method remaining =
      proceed c (fun () -> Generator.remaining c.generator)

    method is_ready =
      proceed c (fun () -> not c.buffering)

    method private get_frame frame =
      proceed c
        (fun () ->
           assert (not c.buffering) ;
           Generator.fill c.generator frame ;
           if Generator.length c.generator = 0 then begin
             self#log#f 3 "Buffer emptied, start buffering..." ;
             c.buffering <- true
           end)

    method abort_track =
      proceed c (fun () -> c.abort <- true)

  end

  class consumer
    ~autostart ~infallible ~on_start ~on_stop ~pre_buffer ~max_buffer
    ~kind source_val c
  =
    let prebuf = Frame.master_of_seconds pre_buffer in
    let maxbuf = Frame.master_of_seconds max_buffer in
  object
    inherit Output.output
      ~output_kind:"buffer" ~content_kind:kind
      ~infallible ~on_start ~on_stop
      source_val
      autostart

    method output_reset = ()
    method output_start = ()
    method output_stop  = ()

    val source = Lang.to_source source_val

    method output_send frame =
      proceed c
        (fun () ->
           if c.abort then begin
             c.abort <- false ;
             source#abort_track
           end ;
           Generator.feed_from_frame c.generator frame ;
           if Generator.length c.generator > prebuf then begin
             c.buffering <- false ;
             if Generator.length c.generator > maxbuf then
               Generator.remove c.generator
                 (Generator.length c.generator - maxbuf)
           end)

  end

  let create ~autostart ~infallible ~on_start ~on_stop
        ~pre_buffer ~max_buffer ~kind source_val =
    let control = {
      generator = Generator.create () ;
      lock = Mutex.create () ;
      buffering = true ;
      abort = false
    } in
    let _ =
      new consumer
        ~autostart ~infallible ~on_start ~on_stop
        ~kind source_val
        ~pre_buffer ~max_buffer control
    in
      new producer ~kind control

end

let () =
  let k = Lang.univ_t 1 in
    Lang.add_operator "buffer"
      (Output.proto @
       ["buffer", Lang.float_t, Some (Lang.float 1.),
          Some "Amount of data to pre-buffer, in seconds." ;
        "max", Lang.float_t, Some (Lang.float 10.),
          Some "Maximum amount of buffered data, in seconds." ;
        "", Lang.source_t k, None, None])
      ~kind:(Lang.Unconstrained k)
      ~category:Lang.Liquidsoap
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
           Buffer.create
             ~infallible ~autostart ~on_start ~on_stop
             ~pre_buffer ~max_buffer ~kind s)

module AdaptativeBuffer =
struct
  module RB = Audio.Ringbuffer
  module MG = Generator.Metadata

  (* The kind of value shared by a producer and a consumer. *)
  (* TODO: also have breaks and metadata as in generators. *)
  type control = {
    lock : Mutex.t;
    rb : RB.t;
    mutable rb_length : float; (* average length of the ringbuffer in samples *)
    mg : MG.t;
    mutable buffering : bool;
    mutable abort : bool;
  }

  let proceed control f =
    Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~kind ~pre_buffer ~averaging c =
    let channels = (Frame.type_of_kind kind).Frame.audio in
    let prebuf = float (Frame.audio_of_seconds pre_buffer) in
    (* see get_frame for an explanation *)
    let alpha = log 2. *. AFrame.duration () /. averaging in
  object (self)
    inherit Source.source kind ~name:"buffer.adaptative_producer"

    method stype = Source.Fallible

    method remaining =
      proceed c (fun () -> MG.remaining c.mg)

    method is_ready =
      proceed c (fun () -> not c.buffering)

    method private get_frame frame =
      proceed c
        (fun () ->
          assert (not c.buffering);

          (* Update the average length of the ringbuffer (with a damping
             coefficient in order not to be too sensitive to quick local
             variations).*)
          (* y: average length, dt: frame duration, x: read length, A=a/dt

             y(t+dt)=(1-a)y(t)+ax(t)
             y'(t)=(a/dt)(x(t)-y(t))
             y'(t)+Ay(t)=Ax(t)

             When x=x0 is constant and we start at y0, the solution is
             y(t) = (y0-x0)exp(-At)+x0

             half-life is at th = ln(2)/A
             we should thus choose alpha = (dt * ln 2)/th
          *)
          c.rb_length <- (1. -. alpha) *. c.rb_length +. alpha *. float (RB.read_space c.rb);

          (* Fill dlen samples of dst using slen samples of the ringbuffer. *)
          let fill dst dofs dlen slen =
            let slen = min slen (RB.read_space c.rb) in
            if slen > 0 then
              let src = Audio.create channels slen in
              RB.read c.rb src 0 slen;
              if slen = dlen then
                Audio.blit src 0 dst dofs slen
              else
                (* TODO: we could do better than nearest interpolation. However,
                   for slight adaptations the difference should not really be
                   audible. *)
                for c = 0 to channels - 1 do
                  let srcc = src.(c) in
                  let dstc = dst.(c) in
                  for i = 0 to dlen - 1 do
                    let x = srcc.(i * slen / dlen) in
                    dstc.(i + dofs) <- x
                  done
                done
          in

          let ofs = Frame.position frame in
          let len =
            let len = Lazy.force Frame.size - ofs in
            let rem = MG.remaining c.mg in
            let rem = if rem = -1 then MG.length c.mg else rem in
            min len rem
          in
          let aofs = Frame.audio_of_master ofs in
          let alen = Frame.audio_of_master len in
          let buf = AFrame.content_of_type ~channels frame aofs in

          (* We scale the reading so that the buffer always approximatively
             contains prebuf data. *)
          let scaling = c.rb_length /. prebuf in
          let scale n = int_of_float (float n *. scaling) in
          let salen = scale alen in
          fill buf aofs alen salen;
          Frame.add_break frame len;

          (* Fill in metadata *)
          let md = MG.metadata c.mg len in
          List.iter (fun (t,m) -> Frame.set_metadata frame (scale t) m) md;
          MG.advance c.mg len;
          if Frame.is_partial frame then MG.drop_initial_break c.mg;

          (* If we should play at 10x we declare that we should buffer again. *)
          if RB.read_space c.rb = 0 || scaling < 0.1 then begin
            self#log#f 3 "Buffer emptied, start buffering...";
            c.buffering <- true
          end)

    method abort_track =
      proceed c (fun () -> c.abort <- true)
  end

  class consumer
    ~autostart ~infallible ~on_start ~on_stop ~pre_buffer
    ~kind source_val c
    =
    let channels = (Frame.type_of_kind kind).Frame.audio in
    let prebuf = Frame.audio_of_seconds pre_buffer in
  object
    inherit Output.output
      ~output_kind:"buffer" ~content_kind:kind
      ~infallible ~on_start ~on_stop
      source_val
      autostart

    method output_reset = ()
    method output_start = ()
    method output_stop  = ()

    val source = Lang.to_source source_val

    method output_send frame =
      proceed c
        (fun () ->
          if c.abort then begin
            c.abort <- false;
            source#abort_track
          end;
          let len = AFrame.position frame in
          (* TODO: is this ok to start from 0? *)
          let buf = AFrame.content_of_type ~channels frame 0 in
          if RB.write_space c.rb < len then RB.read_advance c.rb (len - RB.write_space c.rb);
          RB.write c.rb buf 0 len;
          MG.feed_from_frame c.mg frame;
          if RB.read_space c.rb > prebuf then c.buffering <- false
        )

  end

  let create ~autostart ~infallible ~on_start ~on_stop
      ~pre_buffer ~max_buffer ~averaging ~kind source_val =
    let channels = (Frame.type_of_kind kind).Frame.audio in
    let control =
      {
        lock = Mutex.create ();
        rb = RB.create channels (Frame.audio_of_seconds max_buffer);
        rb_length = float (Frame.audio_of_seconds pre_buffer);
        mg = MG.create ();
        buffering = true;
        abort = false;
      }
    in
    let _ =
      new consumer
        ~autostart ~infallible ~on_start ~on_stop
        ~kind source_val ~pre_buffer control
    in
    new producer ~kind ~pre_buffer ~averaging control
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
  Lang.add_operator "buffer.adaptative"
    (Output.proto @
       ["buffer", Lang.float_t, Some (Lang.float 1.),
        Some "Amount of data to pre-buffer, in seconds.";
        "max", Lang.float_t, Some (Lang.float 10.),
        Some "Maximum amount of buffered data, in seconds.";
        "averaging", Lang.float_t, Some (Lang.float 30.),
        Some "Half-life for the averaging of the buffer size, in seconds.";
        "", Lang.source_t k, None, None])
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Liquidsoap
    ~descr:"Create a buffer between two different clocks. The speed of \
            the output is adapted so that no buffer underrun or overrun \
            occurs. This wonderful behavior has a cost: the pitch of the \
            sound might be changed a little."
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
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      AdaptativeBuffer.create
        ~infallible ~autostart ~on_start ~on_stop
        ~pre_buffer ~max_buffer ~averaging ~kind s)
