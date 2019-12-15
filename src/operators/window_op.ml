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

open Source

type mode = RMS | Peak

class window ~kind mode duration source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  object
    inherit
      operator
        kind [source]
        ~name:(match mode with RMS -> "rms" | Peak -> "peak")

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method seek = source#seek

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    (** Accumulator (e.g. sum of squares). *)
    val acc = Array.make channels 0.

    (** Duration of the accumlated data. *)
    val mutable acc_dur = 0

    (** Last computed value (rms or peak). *)
    val mutable value = Array.make channels 0.

    val m = Mutex.create ()

    method value = Tutils.mutexify m (fun () -> value) ()

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let duration = duration () in
      if duration > 0. then (
        let duration = Frame.audio_of_seconds duration in
        let position = AFrame.position buf in
        let buf = AFrame.content buf offset in
        for i = offset to position - 1 do
          for c = 0 to channels - 1 do
            let x = buf.(c).{i} in
            match mode with
              | RMS -> acc.(c) <- acc.(c) +. (x *. x)
              | Peak -> acc.(c) <- max acc.(c) (abs_float x)
          done;
          acc_dur <- acc_dur + 1;
          if acc_dur >= duration then (
            let dur = float acc_dur in
            let value' =
              Array.init channels (fun i ->
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
            Tutils.mutexify m (fun () -> value <- value') () )
        done )
  end

let declare mode suffix format fun_ret_t f_ans =
  let k = Lang.kind_type_of_kind_format format in
  let return_t = Lang.product_t (Lang.fun_t [] fun_ret_t) (Lang.source_t k) in
  let name = match mode with RMS -> "rms" | Peak -> "peak" in
  let doc = match mode with RMS -> "RMS volume" | Peak -> "peak volume" in
  Lang.add_builtin (name ^ suffix)
    ~category:(Lang.string_of_category Lang.Visualization)
    ~descr:
      ( "Get current " ^ doc
      ^ " of the source. Returns a pair `(f,s)` where s is a new source and \
         `f` is a function of type `() -> float` and returns the current " ^ doc
      ^ " of the source, with `0.0 <= " ^ doc
      ^ " <= 1.0`. Some operators like `amplify` and `compress` can produce \
         amplitudes greater than 1.0 if misconfigured, inducing clipping in \
         the output." )
    [
      ( "id",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Force the value of the source ID." );
      ( "duration",
        Lang.float_getter_t (),
        Some (Lang.float 0.5),
        Some
          "Duration of the window (in seconds). A value <= 0, means that \
           computation should not be performed." );
      ("", Lang.source_t k, None, None);
    ]
    return_t
    (fun p t ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let id = Lang.to_string (f "id") in
      let duration = Lang.to_float_getter (f "duration") in
      let _, t = Lang.of_product_t t in
      let kind = Lang.frame_kind_of_kind_type (Lang.of_source_t t) in
      let s = new window ~kind mode duration src in
      if id <> "" then s#set_id id;
      let f = Lang.val_fun [] ~ret_t:fun_ret_t (fun _ _ -> f_ans s#value) in
      Lang.product f (Lang.source (s :> Source.source)))

let () =
  let mean value =
    let x = Array.fold_left ( +. ) 0. value in
    let x = x /. float (Array.length value) in
    Lang.float x
  in
  let stereo value =
    Lang.product (Lang.float value.(0)) (Lang.float value.(1))
  in
  declare RMS "" Lang.any_fixed Lang.float_t mean;
  declare RMS ".stereo"
    (Lang.any_fixed_with ~audio:2 ())
    (Lang.product_t Lang.float_t Lang.float_t)
    stereo;
  declare Peak "" Lang.any_fixed Lang.float_t mean;
  declare Peak ".stereo"
    (Lang.any_fixed_with ~audio:2 ())
    (Lang.product_t Lang.float_t Lang.float_t)
    stereo
