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

(** Below, lengths are in audio samples, thresholds in RMS (in [0.;1.]). *)

class virtual base ~start_blank ~track_sensitive ~max_blank ~min_noise
  ~threshold =
  object (self)
    (** State can be either
    *  - `Noise l: the source is considered to be emitting,
    *     but it has been silent for l samples;
    *  - `Blank l: the source is considered to be silent,
    *     but it has been noisy for l samples. *)
    val mutable state = if start_blank then `Blank 0 else `Noise 0

    method virtual private log : Log.t

    method private in_blank = match state with `Blank _ -> true | _ -> false

    method private string_of_state =
      function `Blank _ -> "blank" | `Noise _ -> "no blank"

    method private set_state s =
      begin
        match (state, s) with
        | `Blank _, `Noise _ | `Noise _, `Blank _ ->
            self#log#info "Setting state to %s" (self#string_of_state s)
        | _ -> ()
      end;
      state <- s

    (** This method should be called after the frame [s] has been
    * filled, where [p0] is the position in [s] before filling. *)
    method private check_blank s p0 =
      (* TODO The [p0 > 0] condition may not be fully justified.
       *      By the way it was absent in [eat_blank]. *)
      if AFrame.is_partial s || p0 > 0 then (
        if
          (* Don't bother analyzing the end of this track, jump to the new state. *)
          track_sensitive
        then self#set_state (`Noise 0) )
      else (
        let len = AFrame.position s - p0 in
        let rms = AFrame.rms s p0 len in
        let noise =
          Array.fold_left (fun noise r -> noise || r > threshold) false rms
        in
        match state with
          | `Noise blank_len ->
              if noise then (if blank_len <> 0 then self#set_state (`Noise 0))
              else (
                let blank_len = blank_len + len in
                if blank_len <= max_blank then self#set_state (`Noise blank_len)
                else self#set_state (`Blank 0) )
          | `Blank noise_len ->
              if noise then (
                let noise_len = noise_len + len in
                if noise_len < min_noise then self#set_state (`Blank noise_len)
                else self#set_state (`Noise 0) )
              else if noise_len <> 0 then self#set_state (`Blank 0) )
  end

class on_blank ~kind ~start_blank ~max_blank ~min_noise ~threshold
  ~track_sensitive ~on_blank ~on_noise source =
  object (self)
    inherit operator ~name:"on_blank" kind [source]

    inherit base ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold

    method stype = source#stype

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method private get_frame ab =
      let p0 = AFrame.position ab in
      source#get ab;
      let was_blank = self#in_blank in
      let is_blank =
        self#check_blank ab p0;
        self#in_blank
      in
      match (was_blank, is_blank) with
        | true, false -> ignore (Lang.apply ~t:Lang.unit_t on_noise [])
        | false, true -> ignore (Lang.apply ~t:Lang.unit_t on_blank [])
        | _ -> ()
  end

class strip ~kind ~start_blank ~max_blank ~min_noise ~threshold ~track_sensitive
  source =
  object (self)
    (* Stripping is easy:
     *  - declare yourself as unavailable when the source is silent
     *  - keep pulling data from the source during those times. *)
    inherit active_operator ~name:"strip_blank" kind [source]

    inherit base ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold

    initializer
    ns_kind <- "strip_blank";
    let status _ = string_of_bool self#in_blank in
    self#register_command "is_stripping"
      ~descr:"Check if the source is stripping." status

    method stype = Fallible

    method is_ready = (not self#in_blank) && source#is_ready

    method remaining = if self#in_blank then 0 else source#remaining

    method seek n = if self#in_blank then 0 else source#seek n

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    method private get_frame ab =
      let p0 = AFrame.position ab in
      let b0 = AFrame.breaks ab in
      source#get ab;
      self#check_blank ab p0;

      (* It's useless to strip metadata, because [ab] is [memo]
       * and metadata will not be copied from it outside of the track. *)
      if self#in_blank then AFrame.set_breaks ab (p0 :: b0)

    method private output =
      (* We only #get once in memo; this is why we can set_breaks everytime
       * in #get_frame.
       * This behavior makes time flow slower than expected, but doesn't seem
       * harmful. The advantage of doing this is that if stripping stops because
       * the track ends, the beginning of the next track won't be lost. (Because
       * of granularity issues, the change of #is_ready only takes effect at the
       * end of the clock cycle). *)
      if source#is_ready && self#in_blank && AFrame.is_partial memo then
        self#get_frame memo

    method output_reset = ()

    method output_get_ready = ()

    method is_active = true
  end

class eat ~kind ~track_sensitive ~at_beginning ~start_blank ~max_blank
  ~min_noise ~threshold source =
  object (self)
    (* Eating blank is trickier than stripping.
     * TODO It requires control over the time flow of the source; we need
     * to force our own clock onto it. *)
    inherit operator ~name:"eat_blank" kind [source]

    inherit base ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold

    (** We strip when the source is silent,
    * but only at the beginning of tracks if [at_beginning] is passed. *)

    val mutable stripping = false

    val mutable beginning = true

    method stype = Fallible

    method is_ready = source#is_ready

    method remaining = source#remaining

    method seek = source#seek

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    method private get_frame ab =
      let first = ref true in
      let breaks = AFrame.breaks ab in
      (* Do at least one round of pulling data from the source into [ab],
       * and as many as needed for getting rid of silence. *)
      while !first || stripping do
        if not !first then AFrame.set_breaks ab breaks;
        first := false;
        let p0 = AFrame.position ab in
        source#get ab;
        if track_sensitive && AFrame.is_partial ab then (
          stripping <- false;
          beginning <- true );
        let was_blank = self#in_blank in
        let is_blank =
          self#check_blank ab p0;
          self#in_blank
        in
        match (was_blank, is_blank) with
          | false, true ->
              if beginning || not at_beginning then stripping <- true
          | true, false ->
              stripping <- false;
              beginning <- false
          | _ -> ()
      done
  end

let kind = Lang.kind_type_of_kind_format Lang.any

let proto =
  [
    ( "threshold",
      Lang.float_t,
      Some (Lang.float (-40.)),
      Some "Power in decibels under which the stream is considered silent." );
    ( "start_blank",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Start assuming we have blank." );
    ( "max_blank",
      Lang.float_t,
      Some (Lang.float 20.),
      Some "Maximum duration of silence allowed, in seconds." );
    ( "min_noise",
      Lang.float_t,
      Some (Lang.float 0.),
      Some "Minimum duration of noise required to end silence, in seconds." );
    ( "track_sensitive",
      Lang.bool_t,
      Some (Lang.bool true),
      Some "Reset blank counter at each track." );
    ("", Lang.source_t kind, None, None);
  ]

let extract p =
  let f v = List.assoc v p in
  let s = Lang.to_source (f "") in
  let start_blank = Lang.to_bool (f "start_blank") in
  let max_blank =
    let l = Lang.to_float (f "max_blank") in
    Frame.audio_of_seconds l
  in
  let min_noise =
    let l = Lang.to_float (f "min_noise") in
    Frame.audio_of_seconds l
  in
  let threshold =
    let v = f "threshold" in
    let t = Lang.to_float v in
    if t > 0. then
      raise (Lang_errors.Invalid_value (v, "threshold should be negative"));
    Audio.lin_of_dB t
  in
  let ts = Lang.to_bool (f "track_sensitive") in
  (start_blank, max_blank, min_noise, threshold, ts, s)

let () =
  Lang.add_operator "on_blank" ~kind:(Lang.Unconstrained kind)
    ~category:Lang.TrackProcessing
    ~descr:"Calls a given handler when detecting a blank."
    ( ( "",
        Lang.fun_t [] Lang.unit_t,
        None,
        Some "Handler called when blank is detected." )
    :: ( "on_noise",
         Lang.fun_t [] Lang.unit_t,
         Some (Lang.val_cst_fun [] Lang.unit),
         Some "Handler called when noise is detected." )
    :: proto )
    (fun p kind ->
      let on_blank = Lang.assoc "" 1 p in
      let on_noise = Lang.assoc "on_noise" 1 p in
      let p = List.remove_assoc "" p in
      let start_blank, max_blank, min_noise, threshold, track_sensitive, s =
        extract p
      in
      new on_blank
        ~kind ~start_blank ~max_blank ~min_noise ~threshold ~track_sensitive
        ~on_blank ~on_noise s);
  Lang.add_operator "strip_blank" ~active:true ~kind:(Lang.Unconstrained kind)
    ~category:Lang.TrackProcessing
    ~descr:"Make the source unavailable when it is streaming blank." proto
    (fun p kind ->
      let start_blank, max_blank, min_noise, threshold, track_sensitive, s =
        extract p
      in
      ( new strip
          ~kind ~track_sensitive ~start_blank ~max_blank ~min_noise ~threshold s
        :> Source.source ));
  Lang.add_operator "eat_blank" ~kind:(Lang.Unconstrained kind)
    ~category:Lang.TrackProcessing
    ~descr:
      "Eat blanks, i.e., drop the contents of the stream until it is not blank \
       anymore."
    ( ( "at_beginning",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Only eat at the beginning of a track." )
    :: proto )
    (fun p kind ->
      let at_beginning = Lang.to_bool (List.assoc "at_beginning" p) in
      let start_blank, max_blank, min_noise, threshold, track_sensitive, s =
        extract p
      in
      new eat
        ~kind ~at_beginning ~track_sensitive ~start_blank ~max_blank ~min_noise
        ~threshold s)
