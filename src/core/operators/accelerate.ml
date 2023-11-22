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

open Source

class accelerate ~ratio ~randomize source_val =
  let source = Lang.to_source source_val in
  object (self)
    inherit operator ~name:"accelerate" [source] as super
    inherit! Child_support.base ~check_self_sync:true [source_val]
    method self_sync = source#self_sync
    method stype = source#stype
    method seek = source#seek
    method seek_source = source#seek_source

    method remaining =
      let rem = source#remaining in
      if rem = -1 then rem else int_of_float (float rem *. ratio ())

    method abort_track = source#abort_track

    (** Frame used for dropping samples. *)
    val mutable null = Frame.dummy ()

    method! private wake_up x =
      super#wake_up x;
      null <- Frame.create self#content_type;
      source#get_ready [(self :> source)]

    method! private sleep = source#leave (self :> source)
    method private _is_ready = source#is_ready

    (** Filled ticks. *)
    val mutable filled = 0

    (** Skipped ticks. *)
    val mutable skipped = 0

    method private must_drop =
      let ratio = ratio () in
      if ratio <= 1. then false
      else (
        (* How much we filled compared to what we should have. *)
        let d = float filled -. (float (filled + skipped) /. ratio) in
        let d = Frame.seconds_of_main (truncate d) in
        let rnd = randomize () in
        if rnd = 0. then d > 0.
        else (
          let a = d /. rnd in
          (* Scaled logistic function: 0. when a is very negative, 1. when a is
             very positive. *)
          let l = (tanh (a *. 2.) +. 1.) /. 2. in
          Random.float 1. > 1. -. l))

    method private get_frame frame =
      let pos = ref 1 in
      (* Drop frames if we are late. *)
      (* TODO: we could also duplicate if we are in advance. *)
      while !pos > 0 && self#must_drop && source#is_ready ~frame () do
        Frame.clear null;
        self#child_on_output (fun () -> source#get null);
        pos := Frame.position null;
        skipped <- skipped + !pos
      done;
      if source#is_ready ~frame () then (
        let before = Frame.position frame in
        if !pos > 0 then self#child_on_output (fun () -> source#get frame);
        let after = Frame.position frame in
        filled <- filled + (after - before))
      else Frame.add_break frame (Frame.position frame)
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator "accelerate"
    [
      ( "ratio",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 2.),
        Some "A value higher than 1 means speeding up." );
      ( "randomize",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "Randomization (0 means no randomization)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~flags:[`Experimental; `Extra] ~return_t ~category:`Audio
    ~descr:
      "Accelerate a stream by dropping frames. This is useful for testing \
       scripts."
    (fun p ->
      let f v = List.assoc v p in
      let src = f "" in
      let ratio = Lang.to_float_getter (f "ratio") in
      let randomize = Lang.to_float_getter (f "randomize") in
      new accelerate ~ratio ~randomize src)
