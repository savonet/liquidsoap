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

module MG = struct
  type t = {
    mutable metadata : (int * Frame.metadata) list;
    mutable breaks : int list;
    mutable length : int;
  }

  let create () = { metadata = []; breaks = []; length = 0 }

  let clear g =
    g.metadata <- [];
    g.breaks <- [];
    g.length <- 0

  let advance g len =
    g.metadata <- List.map (fun (t, m) -> (t - len, m)) g.metadata;
    g.metadata <- List.filter (fun (t, _) -> t >= 0) g.metadata;
    g.breaks <- List.map (fun t -> t - len) g.breaks;
    g.breaks <- List.filter (fun t -> t >= 0) g.breaks;
    g.length <- g.length - len;
    assert (g.length >= 0)

  let length g = g.length
  let remaining g = match g.breaks with a :: _ -> a | _ -> -1
  let metadata g len = List.filter (fun (t, _) -> t < len) g.metadata

  let feed_from_frame g frame =
    let size = Lazy.force Frame.size in
    let length = length g in
    g.metadata <-
      g.metadata
      @ List.map (fun (t, m) -> (length + t, m)) (Frame.get_all_metadata frame);
    g.breaks <-
      g.breaks
      @ List.map
          (fun t -> length + t)
          (* Filter out the last break, which only marks the end of frame, not a
           * track limit (doesn't mean is_partial). *)
          (List.filter (fun x -> x < size) (Frame.breaks frame));
    let frame_length =
      let rec aux = function [t] -> t | _ :: tl -> aux tl | [] -> size in
      aux (Frame.breaks frame)
    in
    g.length <- g.length + frame_length

  let drop_initial_break g =
    match g.breaks with
      | 0 :: tl -> g.breaks <- tl
      | [] -> () (* end of stream / underrun... *)
      | _ -> assert false

  let fill g frame =
    let offset = Frame.position frame in
    let needed =
      let size = Lazy.force Frame.size in
      let remaining = remaining g in
      let remaining = if remaining = -1 then length g else remaining in
      min (size - offset) remaining
    in
    List.iter
      (fun (p, m) -> if p < needed then Frame.set_metadata frame (offset + p) m)
      g.metadata;
    advance g needed;

    (* Mark the end of this filling. If the frame is partial it must be because
     * of a break in the generator, or because the generator is emptying.
     * Conversely, each break in the generator must cause a partial frame, so
     * don't remove any if it isn't partial. *)
    Frame.add_break frame (offset + needed);
    if Frame.is_partial frame then drop_initial_break g
end

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
  (* The kind of value shared by a producer and a consumer. *)
  type control = {
    lock : Mutex.t;
    generator : Generator.t;
    mutable buffering : bool;
    mutable abort : bool;
  }

  let proceed control f = Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~id c =
    object (self)
      inherit Source.source ~name:id ()
      method self_sync = (`Static, false)
      method stype = `Fallible
      method remaining = proceed c (fun () -> Generator.remaining c.generator)
      method is_ready = proceed c (fun () -> not c.buffering)

      method seek len =
        let len = min (Generator.length c.generator) len in
        Generator.truncate c.generator len;
        len

      method buffer_length = Generator.length c.generator

      method private get_frame frame =
        proceed c (fun () ->
            assert (not c.buffering);
            Generator.fill c.generator frame;
            if Frame.is_partial frame && Generator.length c.generator = 0 then (
              self#log#important "Buffer emptied, start buffering...";
              c.buffering <- true))

      method abort_track = proceed c (fun () -> c.abort <- true)
    end

  class consumer ~id ~autostart ~infallible ~on_start ~on_stop ~pre_buffer
    ~max_buffer source_val c =
    let prebuf = Frame.main_of_seconds pre_buffer in
    let maxbuf = Frame.main_of_seconds max_buffer in
    object
      inherit
        Output.output
          ~output_kind:id ~infallible ~on_start ~on_stop source_val autostart

      method! reset = ()
      method start = ()
      method stop = ()
      val source = Lang.to_source source_val

      method send_frame frame =
        proceed c (fun () ->
            if c.abort then (
              c.abort <- false;
              source#abort_track);
            Generator.feed c.generator frame;
            if Generator.length c.generator > prebuf then (
              c.buffering <- false;
              if Generator.length c.generator > maxbuf then
                Generator.truncate c.generator
                  (Generator.length c.generator - maxbuf)))
    end

  let create ~id ~autostart ~infallible ~on_start ~on_stop ~pre_buffer
      ~max_buffer source_val =
    let control =
      {
        generator = Generator.create (Lang.to_source source_val)#content_type;
        lock = Mutex.create ();
        buffering = true;
        abort = false;
      }
    in
    let _ =
      new consumer
        ~id:(Printf.sprintf "%s.consumer" id)
        ~autostart ~infallible ~on_start ~on_stop source_val ~pre_buffer
        ~max_buffer control
    in
    new producer ~id:(Printf.sprintf "%s.producer" id) control
end

let buffer =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator "buffer"
    ([
       ( "fallible",
         Lang.bool_t,
         Some (Lang.bool true),
         Some "Allow the child source to fail." );
     ]
    @ List.filter (fun (lbl, _, _, _) -> lbl <> "fallible") Output.proto
    @ [
        ( "buffer",
          Lang.float_t,
          Some (Lang.float 1.),
          Some "Amount of data to pre-buffer, in seconds." );
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum amount of buffered data, in seconds." );
        ("", Lang.source_t frame_t, None, None);
      ])
    ~return_t:frame_t ~category:`Liquidsoap
    ~meth:
      [
        ( "buffer_length",
          ([], Lang.fun_t [] Lang.int_t),
          "Buffer length, in main ticks",
          fun s -> Lang.val_fun [] (fun _ -> Lang.int s#buffer_length) );
      ]
    ~descr:"Create a buffer between two different clocks."
    (fun p ->
      let id =
        Lang.to_default_option ~default:"buffer" Lang.to_string
          (List.assoc "id" p)
      in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start = List.assoc "on_start" p in
      let on_stop = List.assoc "on_stop" p in
      let on_start () = ignore (Lang.apply on_start []) in
      let on_stop () = ignore (Lang.apply on_stop []) in
      let s = List.assoc "" p in
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      Buffer.create ~id ~infallible ~autostart ~on_start ~on_stop ~pre_buffer
        ~max_buffer s)

module AdaptativeBuffer = struct
  (** Ringbuffers where number of channels is fixed on first write. *)
  module RB = struct
    module RB = Audio.Ringbuffer

    type t = {
      size : int; (* size of the ringbuffer (in samples) *)
      mutable rb : RB.t option (* the ringbuffer *);
    }

    let create size = { size; rb = None }
    let read_space r = match r.rb with Some rb -> RB.read_space rb | None -> 0
    let read r buf = RB.read (Option.get r.rb) buf
    let read_advance r n = RB.read_advance (Option.get r.rb) n

    let write_space r =
      match r.rb with Some rb -> RB.write_space rb | None -> r.size

    let rec write r buf =
      match r.rb with
        | Some rb -> RB.write rb buf
        | None ->
            r.rb <- Some (RB.create (Audio.channels buf) r.size);
            write r buf
  end

  (* TODO: also have breaks and metadata as in generators. *)

  (** The kind of value shared by a producer and a consumer. *)
  type control = {
    lock : Mutex.t;
        (* this mutex must be taken before accessing any other field *)
    rb : RB.t; (* the ringbuffer *)
    mutable rb_length : float;
        (* average length of the ringbuffer, in samples *)
    mg : MG.t; (* metadata generator *)
    mutable buffering : bool;
        (* when true we are buffering: filling the buffer, but not reading from it *)
    mutable abort : bool; (* whether we asked to abort the current track *)
  }

  let proceed control f = Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~pre_buffer ~averaging ~limit ~resample c =
    let prebuf = float (Frame.audio_of_seconds pre_buffer) in
    (* Nice enough approximation of the factor when the time constant is large
       compared to the frame duration, see
       https://en.wikipedia.org/wiki/Exponential_smoothing#Time_constant *)
    let alpha = AFrame.duration () /. averaging in
    object (self)
      inherit Source.source ~name:"buffer.adaptative.producer" () as super
      inherit Source.no_seek
      method stype = `Fallible
      method self_sync = (`Static, false)
      method remaining = proceed c (fun () -> MG.remaining c.mg)
      method is_ready = proceed c (fun () -> not c.buffering)
      method ratio = proceed c (fun () -> c.rb_length /. prebuf)

      method buffer_duration =
        proceed c (fun () -> Frame.seconds_of_audio (RB.read_space c.rb))

      method buffer_estimated_duration =
        proceed c (fun () -> Frame.seconds_of_audio (int_of_float c.rb_length))

      val mutable converter = None

      method! wake_up a =
        super#wake_up a;
        if resample then
          converter <-
            Some (Audio_converter.Samplerate.create self#audio_channels)

      method private get_frame frame =
        proceed c (fun () ->
            assert (not c.buffering);

            (* Update the average length of the ringbuffer (with a damping
               coefficient in order not to be too sensitive to quick local
               variations). We use exponential smoothing here:
               https://en.wikipedia.org/wiki/Exponential_smoothing *)
            c.rb_length <-
              ((1. -. alpha) *. c.rb_length)
              +. (alpha *. float (RB.read_space c.rb));

            (* Limit estimation *)
            c.rb_length <- min c.rb_length (prebuf *. limit);
            c.rb_length <- max c.rb_length (prebuf /. limit);

            (* Fill dlen samples of dst using slen samples of the ringbuffer. *)
            let fill dst dofs dlen slen =
              (* TODO: when the RB is low on space we'd better not fill the whole
                 frame *)
              let slen = min slen (RB.read_space c.rb) in
              if slen > 0 then (
                match converter with
                  | Some converter ->
                      let src = Audio.create self#audio_channels slen in
                      RB.read c.rb src;
                      let ratio = float dlen /. float slen in
                      let buf, off, len =
                        Audio_converter.Samplerate.resample converter ratio src
                          0 slen
                      in
                      if len <> dlen then
                        self#log#important
                          "Unexpected length after resampling: %d instead of %d"
                          len dlen;
                      let len = min len dlen in
                      Audio.blit buf off dst dofs len;
                      (* In case we are too short, duplicate samples. *)
                      if len > 0 then
                        for i = dofs + len to dofs + dlen - 1 do
                          for c = 0 to Array.length dst - 1 do
                            dst.(c).(i) <- dst.(c).(len - 1)
                          done
                        done
                  | None ->
                      let src = Audio.create self#audio_channels slen in
                      RB.read c.rb src;
                      if slen = dlen then Audio.blit src 0 dst dofs slen
                      else
                        (* TODO: we could do better than nearest interpolation. However,
                           for slight adaptations the difference should not really be
                           audible. *)
                        for c = 0 to self#audio_channels - 1 do
                          let srcc = src.(c) in
                          let dstc = dst.(c) in
                          for i = 0 to dlen - 1 do
                            let x = srcc.(i * slen / dlen) in
                            dstc.(i + dofs) <- x
                          done
                        done)
            in
            (* We scale the reading so that the buffer always approximately
               contains prebuf data. *)
            let scaling = c.rb_length /. prebuf in
            let scale n = int_of_float (float n *. scaling) in
            let unscale n = int_of_float (float n /. scaling) in
            let ofs = Frame.position frame in
            let len = Lazy.force Frame.size - ofs in
            let aofs = Frame.audio_of_main ofs in
            let alen = Frame.audio_of_main len in
            let buf = AFrame.pcm frame in
            let salen = scale alen in
            fill buf aofs alen salen;
            Frame.add_break frame (ofs + len);

            (* self#log#debug "filled %d from %d (x %f)" len ofs scaling; *)

            (* Fill in metadata *)
            let md = MG.metadata c.mg (scale len) in
            List.iter (fun (t, m) -> Frame.set_metadata frame (unscale t) m) md;
            MG.advance c.mg (min (Frame.main_of_audio salen) (MG.length c.mg));
            if Frame.is_partial frame then MG.drop_initial_break c.mg;

            (* If there is no data left, we should buffer again. *)
            if RB.read_space c.rb = 0 then (
              self#log#important "Buffer emptied, start buffering...";
              self#log#debug "Current scaling factor is x%f." scaling;
              MG.advance c.mg (MG.length c.mg);
              c.buffering <- true))

      method abort_track = proceed c (fun () -> c.abort <- true)
    end

  class consumer ~autostart ~infallible ~on_start ~on_stop ~pre_buffer ~reset
    source_val c =
    let prebuf = Frame.audio_of_seconds pre_buffer in
    object (self)
      inherit
        Output.output
          ~output_kind:"buffer" ~name:"buffer.adaptative.consumer" ~infallible
            ~on_start ~on_stop source_val autostart

      method! reset = ()
      method start = ()
      method stop = ()
      val source = Lang.to_source source_val

      method send_frame frame =
        proceed c (fun () ->
            if c.abort then (
              c.abort <- false;
              source#abort_track);
            let len = AFrame.position frame in
            let buf = AFrame.pcm frame in
            if RB.write_space c.rb < len then (
              (* Not enough write space, let's drop a frame. *)
              self#log#important "Buffer full, dropping a frame.";
              RB.read_advance c.rb len;
              MG.advance c.mg (Frame.main_of_audio len));
            RB.write c.rb (Audio.sub buf 0 len);
            MG.feed_from_frame c.mg frame;
            if RB.read_space c.rb > prebuf then (
              if c.buffering && reset then
                c.rb_length <- float (Frame.audio_of_seconds pre_buffer);
              c.buffering <- false))
    end

  let create ~autostart ~infallible ~on_start ~on_stop ~pre_buffer ~max_buffer
      ~averaging ~limit ~reset ~resample source_val =
    let control =
      {
        lock = Mutex.create ();
        rb = RB.create (Frame.audio_of_seconds max_buffer);
        rb_length = float (Frame.audio_of_seconds pre_buffer);
        mg = MG.create ();
        buffering = true;
        abort = false;
      }
    in
    let _ =
      new consumer
        ~autostart ~infallible ~on_start ~on_stop source_val ~pre_buffer ~reset
        control
    in
    new producer ~pre_buffer ~averaging ~limit ~resample control
end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:buffer "adaptative"
    (Output.proto
    @ [
        ( "buffer",
          Lang.float_t,
          Some (Lang.float 1.),
          Some "Amount of data to prebuffer, in seconds." );
        ( "max",
          Lang.float_t,
          Some (Lang.float 10.),
          Some "Maximum amount of buffered data, in seconds." );
        ( "averaging",
          Lang.float_t,
          Some (Lang.float 30.),
          Some
            "Length of the buffer averaging, in seconds (the time constant of \
             the smoothing to be precise). The greater this is, the less \
             reactive to local variations we are." );
        ( "limit",
          Lang.float_t,
          Some (Lang.float 1.25),
          Some
            "Maximum acceleration or deceleration factor, ie how fast or slow \
             we can be compared to realtime." );
        ( "reset",
          Lang.bool_t,
          Some (Lang.bool false),
          Some
            "Reset speed estimation to 1 when the source becomes available \
             again (resuming from a buffer underflow)." );
        ( "resample",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Use proper resampling instead of simply duplicating samples." );
        ("", Lang.source_t frame_t, None, None);
      ])
    ~meth:
      [
        ( "duration",
          ([], Lang.fun_t [] Lang.float_t),
          "Current buffer duration, in seconds.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#buffer_duration) );
        ( "estimated",
          ([], Lang.fun_t [] Lang.float_t),
          "Current smoothed buffer duration, in seconds.",
          fun s ->
            Lang.val_fun [] (fun _ -> Lang.float s#buffer_estimated_duration) );
        ( "ratio",
          ([], Lang.fun_t [] Lang.float_t),
          "Get the current scaling ratio.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#ratio) );
      ]
    ~return_t:frame_t ~category:`Liquidsoap
    ~descr:
      "Create a buffer between two different clocks. The speed of the output \
       is adapted so that no buffer underrun or overrun occurs. This wonderful \
       behavior has a cost: the pitch of the sound might be changed a little."
    ~flags:[`Experimental]
    (fun p ->
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let on_start = List.assoc "on_start" p in
      let on_stop = List.assoc "on_stop" p in
      let on_start () = ignore (Lang.apply on_start []) in
      let on_stop () = ignore (Lang.apply on_stop []) in
      let s = List.assoc "" p in
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let averaging = Lang.to_float (List.assoc "averaging" p) in
      let limit = Lang.to_float (List.assoc "limit" p) in
      let limit = if limit < 1. then 1. /. limit else limit in
      let reset = Lang.to_bool (List.assoc "reset" p) in
      let resample = List.assoc "resample" p |> Lang.to_bool in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      AdaptativeBuffer.create ~infallible ~autostart ~on_start ~on_stop
        ~pre_buffer ~max_buffer ~averaging ~limit ~reset ~resample s)
