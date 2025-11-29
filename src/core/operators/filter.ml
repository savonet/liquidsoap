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

type mode = Low_pass | High_pass | Band_pass | Notch

class filter (source : source) freq q wet mode =
  let rate = float (Lazy.force Frame.audio_rate) in
  object (self)
    inherit operator ~name:"filter" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    val mutable low = [||]
    val mutable high = [||]
    val mutable band = [||]
    val mutable notch = [||]

    initializer
      self#on_wake_up (fun () ->
          let channels = self#audio_channels in
          low <- Array.make channels 0.;
          high <- Array.make channels 0.;
          band <- Array.make channels 0.;
          notch <- Array.make channels 0.)

    (* State vartiable filter, see
       http://www.musicdsp.org/archive.php?classid=3#23

       TODO: the problem with this filter is that it only handles freq <= rate/4,
       we have to find something better. See
       http://www.musicdsp.org/showArchiveComment.php?ArchiveID=23

       Maybe should we implement Chamberlin's version instead, which handles freq
       <= rate/2. See http://www.musicdsp.org/archive.php?classid=3#142 *)
    method private generate_frame =
      let c = source#get_mutable_content Frame.Fields.audio in
      let b = Content.Audio.get_data c in
      let position = source#frame_audio_position in
      let freq = freq () in
      let q = q () in
      let wet = wet () in
      let f = 2. *. sin (Float.pi *. freq /. rate) in
      for c = 0 to Array.length b - 1 do
        let b_c = b.(c) in
        for i = 0 to position - 1 do
          low.(c) <- low.(c) +. (f *. band.(c));
          high.(c) <- (q *. b_c.(i)) -. low.(c) -. (q *. band.(c));
          band.(c) <- (f *. high.(c)) +. band.(c);
          notch.(c) <- high.(c) +. low.(c);
          b_c.(i) <-
            (wet
            *.
              match mode with
              | Low_pass -> low.(c)
              | High_pass -> high.(c)
              | Band_pass -> band.(c)
              | Notch -> notch.(c))
            +. ((1. -. wet) *. b_c.(i))
        done
      done;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

let filter =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "filter"
    [
      ( "freq",
        Lang.getter_t Lang.float_t,
        None,
        Some "Characteristic frequency of the filter." );
      ("q", Lang.getter_t Lang.float_t, Some (Lang.float 1.), None);
      ( "mode",
        Lang.string_t,
        None,
        Some
          "Available modes are 'low' (for low-pass filter), 'high' (for \
           high-pass filter), 'band' (for band-pass filter) and 'notch' (for \
           notch / band-stop / band-rejection filter)." );
      ( "wetness",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some
          "How much of the original signal should be added (1. means only \
           filtered and 0. means only original signal)." );
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio
    ~descr:
      "Perform several kinds of filtering on the signal. Only frequencies \
       below the sampling rate / 4 (generally 10 kHz) are handled well for the \
       `freq` parameter."
    (fun p ->
      let f v = List.assoc v p in
      let freq, q, wet, mode, src =
        ( Lang.to_float_getter (f "freq"),
          Lang.to_float_getter (f "q"),
          Lang.to_float_getter (f "wetness"),
          f "mode",
          Lang.to_source (f "") )
      in
      let mode =
        match Lang.to_string mode with
          | "low" -> Low_pass
          | "high" -> High_pass
          | "band" -> Band_pass
          | "notch" -> Notch
          | _ ->
              raise
                (Error.Invalid_value
                   (mode, "valid values are low|high|band|notch"))
      in
      (new filter src freq q wet mode :> Source.source))
