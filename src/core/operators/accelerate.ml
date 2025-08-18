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

class accelerate ~ratio ~randomize source_val =
  let source = Lang.to_source source_val in
  object (self)
    inherit operator ~name:"accelerate" []
    inherit Child_support.base [source_val]
    method self_sync = source#self_sync
    method fallible = source#fallible
    method seek_source = source#seek_source

    method remaining =
      let rem = source#remaining in
      if rem = -1 then rem else int_of_float (float rem *. ratio ())

    method abort_track = source#abort_track
    method private can_generate_frame = source#is_ready

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

    method private generate_frame =
      let pos = ref 1 in
      (* Drop frames if we are late. *)
      (* TODO: we could also duplicate if we are in advance. *)
      while !pos > 0 && self#must_drop && source#is_ready do
        self#on_child_tick (fun () ->
            if source#is_ready then pos := Frame.position source#get_frame);
        skipped <- skipped + !pos
      done;
      let buf = ref self#empty_frame in
      if source#is_ready then (
        self#on_child_tick (fun () ->
            if source#is_ready then buf := source#get_frame);
        filled <- filled + Frame.position !buf);
      !buf
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
