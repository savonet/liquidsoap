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

open Source

type mode = RMS | Peak

class window mode duration source =
  object (self)
    inherit
      operator [source] ~name:(match mode with RMS -> "rms" | Peak -> "peak")

    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method effective_source = source#effective_source
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    (** Accumulator (e.g. sum of squares). *)
    val mutable acc = [||]

    (** Duration of the accumulated data (in samples). *)
    val mutable acc_dur = 0

    (** Last computed value (rms or peak). *)
    val mutable value = [||]

    initializer
      self#on_wake_up (fun () ->
          let channels = self#audio_channels in
          acc <- Array.make channels 0.;
          value <- Array.make channels 0.)

    val m = Mutex.create ()
    method value = Mutex_utils.mutexify m (fun () -> value) ()

    method private generate_frame =
      let frame = source#get_frame in
      let duration = duration () in
      if duration > 0. then (
        let duration = Frame.audio_of_seconds duration in
        let position = AFrame.position frame in
        let buf = AFrame.pcm frame in
        for i = 0 to position - 1 do
          for c = 0 to self#audio_channels - 1 do
            let x = buf.(c).(i) in
            match mode with
              | RMS -> acc.(c) <- acc.(c) +. (x *. x)
              | Peak -> acc.(c) <- max acc.(c) (Utils.abs_float x)
          done;
          acc_dur <- acc_dur + 1;
          if acc_dur >= duration then (
            let dur = float acc_dur in
            let value' =
              Array.init self#audio_channels (fun i ->
                  match mode with
                    | RMS ->
                        let v = sqrt (acc.(i) /. dur) in
                        acc.(i) <- 0.;
                        v
                    | Peak ->
                        let v = acc.(i) in
                        acc.(i) <- 0.;
                        v)
            in
            acc_dur <- 0;
            Mutex_utils.mutexify m (fun () -> value <- value') ())
        done);
      frame
  end

let declare ?base mode name frame_t fun_ret_t f_ans =
  let meth, doc =
    match mode with
      | RMS -> ("rms", "RMS volume")
      | Peak -> ("peak", "peak volume")
  in
  let return_t = frame_t () in
  Lang.add_operator ?base name ~category:`Audio
    ~meth:
      [
        {
          Lang.name = meth;
          scheme = ([], Lang.fun_t [] fun_ret_t);
          descr = "Current value for the " ^ doc ^ ".";
          value = (fun s -> Lang.val_fun [] (fun _ -> f_ans s#value));
        };
      ]
    ~return_t
    ~descr:
      ("Get current " ^ doc
     ^ " of the source. Returns the source with a method `" ^ meth
     ^ "` to compute the current " ^ doc ^ " of the source, with `0.0 <= " ^ doc
     ^ " <= 1.0`.")
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.5),
        Some
          "Duration of the window (in seconds). A value <= 0, means that \
           computation should not be performed." );
      ("", Lang.source_t return_t, None, None);
    ]
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let duration = Lang.to_float_getter (f "duration") in
      new window mode duration src)

let rms, peak =
  let mean value =
    let x = Array.fold_left ( +. ) 0. value in
    let x = x /. float (Array.length value) in
    Lang.float x
  in
  let stereo value =
    Lang.product (Lang.float value.(0)) (Lang.float value.(1))
  in
  let pcm_t () =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  let stereo_t () =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio_stereo ()) ())
  in
  let rms = declare RMS "rms" pcm_t Lang.float_t mean in
  let peak = declare Peak "peak" pcm_t Lang.float_t mean in
  let declare mode suffix frame_t fun_ret_t f_ans =
    let base = match mode with RMS -> rms | Peak -> peak in
    ignore (declare ~base mode suffix frame_t fun_ret_t f_ans)
  in
  declare RMS "stereo" stereo_t
    (Lang.product_t Lang.float_t Lang.float_t)
    stereo;
  declare Peak "stereo" stereo_t
    (Lang.product_t Lang.float_t Lang.float_t)
    stereo;
  (rms, peak)
