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
open Source

(** Below, lengths are in audio samples, thresholds in RMS (in [0.;1.]). *)

class virtual base ~start_blank ~track_sensitive ~max_blank ~min_noise
  ~threshold =
  object (self)
    (** State can be either
        - `Noise l: the source is considered to be emitting, but it has been
          silent for l samples;
        - `Blank l: the source is considered to be silent, but it has been noisy
          for l samples. *)
    val state = Atomic.make (if start_blank then `Blank 0 else `Noise 0)

    val dB_levels = Atomic.make None
    method dB_levels = Atomic.get dB_levels
    method virtual private log : Log.t

    method is_blank =
      match Atomic.get state with `Blank _ -> true | _ -> false

    method private string_of_state =
      function `Blank _ -> "blank" | `Noise _ -> "no blank"

    method private set_state s =
      begin
        match (Atomic.get state, s) with
          | `Blank _, `Noise _ | `Noise _, `Blank _ ->
              self#log#info "Setting state to %s" (self#string_of_state s)
          | _ -> ()
      end;
      Atomic.set state s

    (** This method should be called after the frame [s] has been filled, where
        [p0] is the position in [s] before filling. *)
    method private check_blank s =
      if Frame.track_marks s <> [] then (
        if
          (* Don't bother analyzing the end of this track, jump to the new state. *)
          track_sensitive ()
        then self#set_state (`Noise 0))
      else (
        let len = AFrame.position s in
        let rms = AFrame.rms s 0 len in
        Atomic.set dB_levels (Some rms);
        let threshold = threshold () in
        let noise =
          Array.fold_left (fun noise r -> noise || r > threshold) false rms
        in
        match Atomic.get state with
          | `Noise blank_len ->
              if noise then (if blank_len <> 0 then self#set_state (`Noise 0))
              else (
                let blank_len = blank_len + len in
                if blank_len <= max_blank () then
                  self#set_state (`Noise blank_len)
                else self#set_state (`Blank 0))
          | `Blank noise_len ->
              if noise then (
                let noise_len = noise_len + len in
                if noise_len < min_noise () then
                  self#set_state (`Blank noise_len)
                else self#set_state (`Noise 0))
              else if noise_len <> 0 then self#set_state (`Blank 0))
  end

class detect ~start_blank ~max_blank ~min_noise ~threshold ~track_sensitive
  source =
  object (self)
    inherit operator ~name:"blank.detect" [source]
    inherit base ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method remaining = source#remaining
    method seek_source = source#seek_source
    method self_sync = source#self_sync
    val mutable on_blank = []
    method on_blank fn = on_blank <- on_blank @ [fn]
    val mutable on_noise = []
    method on_noise fn = on_noise <- on_noise @ [fn]

    method private generate_frame =
      let buf = source#get_frame in
      let was_blank = self#is_blank in
      let is_blank =
        self#check_blank buf;
        self#is_blank
      in
      (match (was_blank, is_blank) with
        | true, false -> List.iter (fun fn -> fn ()) on_noise
        | false, true -> List.iter (fun fn -> fn ()) on_blank
        | _ -> ());
      buf
  end

class strip ~start_blank ~max_blank ~min_noise ~threshold ~track_sensitive
  source =
  object (self)
    (* Stripping is easy:
       - declare yourself as unavailable when the source is silent
       - keep pulling data from the source during those times. *)
    inherit active_operator ~name:"blank.strip" [source]
    inherit base ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold
    method fallible = true

    method private can_generate_frame =
      (* This needs to be computed at all times as it makes sure that the
         source is ready to be ready from in [#output]. *)
      let is_source_ready = source#is_ready in
      (not self#is_blank) && is_source_ready

    method remaining = if self#is_blank then 0 else source#remaining

    method seek_source =
      if self#is_blank then (self :> Source.source) else source#seek_source

    method abort_track = source#abort_track
    method self_sync = source#self_sync
    method private generate_frame = source#get_frame

    method private output =
      if source#is_ready then self#check_blank source#get_frame

    method reset = ()
  end

class eat ~track_sensitive ~at_beginning ~start_blank ~max_blank ~min_noise
  ~threshold source_val =
  let source = Lang.to_source source_val in
  object (self)
    (* Eating blank is trickier than stripping. *)
    inherit operator ~name:"blank.eat" []
    inherit base ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold
    inherit Child_support.base ~check_self_sync:true [source_val]

    (** We strip when the source is silent, but only at the beginning of tracks
        if [at_beginning] is passed. *)

    val mutable stripping = false
    val mutable beginning = true
    method fallible = true
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method seek_source = source#seek_source
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private generate_frame =
      let first = ref true in
      let frame = ref self#empty_frame in
      while source#is_ready && (!first || stripping) do
        first := false;
        self#on_child_tick (fun () ->
            if source#is_ready then frame := source#get_frame);
        let frame = !frame in
        if track_sensitive () && Frame.track_marks frame <> [] then (
          stripping <- false;
          beginning <- true);
        let was_blank = self#is_blank in
        let is_blank =
          self#check_blank frame;
          self#is_blank
        in
        match (was_blank, is_blank) with
          | false, true ->
              if beginning || not at_beginning then stripping <- true
          | true, false ->
              stripping <- false;
              beginning <- false
          | _ -> ()
      done;
      !frame
  end

let proto frame_t =
  [
    ( "threshold",
      Lang.getter_t Lang.float_t,
      Some (Lang.float (-40.)),
      Some "Power in decibels under which the stream is considered silent." );
    ( "start_blank",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Start assuming we have blank." );
    ( "max_blank",
      Lang.getter_t Lang.float_t,
      Some (Lang.float 20.),
      Some "Maximum duration of silence allowed, in seconds." );
    ( "min_noise",
      Lang.getter_t Lang.float_t,
      Some (Lang.float 0.),
      Some "Minimum duration of noise required to end silence, in seconds." );
    ( "track_sensitive",
      Lang.getter_t Lang.bool_t,
      Some (Lang.bool true),
      Some "Reset blank counter at each track." );
    ("", Lang.source_t frame_t, None, None);
  ]

let extract p =
  let f v = List.assoc v p in
  let s = f "" in
  let start_blank = Lang.to_bool (f "start_blank") in
  let max_blank =
    let l = Lang.to_float_getter (f "max_blank") in
    fun () -> Frame.audio_of_seconds (l ())
  in
  let min_noise =
    let l = Lang.to_float_getter (f "min_noise") in
    fun () -> Frame.audio_of_seconds (l ())
  in
  let threshold =
    let v = f "threshold" in
    let t = Lang.to_float_getter v in
    fun () ->
      let t = t () in
      if t > 0. then
        raise (Error.Invalid_value (v, "threshold should be negative"));
      Audio.lin_of_dB t
  in
  let ts = Lang.to_bool_getter (f "track_sensitive") in
  (start_blank, max_blank, min_noise, threshold, ts, s)

let meth () =
  [
    {
      Lang.name = "dB_levels";
      scheme = ([], Lang.fun_t [] (Lang.nullable_t (Lang.list_t Lang.float_t)));
      descr = "Return the detected dB level for each channel.";
      value =
        (fun s ->
          Lang.val_fun [] (fun _ ->
              match s#dB_levels with
                | None -> Lang.null
                | Some lvl ->
                    Lang.list
                      Array.(
                        to_list
                          (map (fun v -> Lang.float (Audio.dB_of_lin v)) lvl))));
    };
    {
      Lang.name = "is_blank";
      scheme = ([], Lang.fun_t [] Lang.bool_t);
      descr = "Indicate whether blank was detected.";
      value = (fun s -> Lang.val_fun [] (fun _ -> Lang.bool s#is_blank));
    };
  ]

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Blank.blank "detect" ~return_t:frame_t
    ~category:`Track ~meth:(meth ())
    ~descr:"Calls a given handler when detecting a blank."
    ~callbacks:
      [
        {
          name = "on_blank";
          params = [];
          descr = "when detecting a blank.";
          register_deprecated_argument = false;
          arg_t = [];
          register = (fun ~params:_ s f -> s#on_blank (fun () -> f []));
        };
        {
          name = "on_noise";
          params = [];
          descr = "when noise is detected.";
          register_deprecated_argument = false;
          arg_t = [];
          register = (fun ~params:_ s f -> s#on_noise (fun () -> f []));
        };
      ]
    (proto frame_t)
    (fun p ->
      let start_blank, max_blank, min_noise, threshold, track_sensitive, s =
        extract p
      in
      new detect
        ~start_blank ~max_blank ~min_noise ~threshold ~track_sensitive
        (Lang.to_source s))

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Blank.blank "strip" ~return_t:frame_t ~meth:(meth ())
    ~category:`Track
    ~descr:
      "Make the source unavailable when it is streaming blank. This is an \
       active operator, meaning that the source used in this operator will be \
       consumed continuously, even when it is not actively used."
    (proto frame_t) (fun p ->
      let start_blank, max_blank, min_noise, threshold, track_sensitive, s =
        extract p
      in
      new strip
        ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold
        (Lang.to_source s))

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Blank.blank "eat" ~return_t:frame_t ~category:`Track
    ~meth:(meth ())
    ~descr:
      "Eat blanks, i.e., drop the contents of the stream until it is not blank \
       anymore."
    (( "at_beginning",
       Lang.bool_t,
       Some (Lang.bool false),
       Some "Only eat at the beginning of a track." )
    :: proto frame_t)
    (fun p ->
      let at_beginning = Lang.to_bool (List.assoc "at_beginning" p) in
      let start_blank, max_blank, min_noise, threshold, track_sensitive, s =
        extract p
      in
      new eat
        ~at_beginning ~track_sensitive ~start_blank ~max_blank ~min_noise
        ~threshold s)
