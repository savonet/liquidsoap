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

class normalize ~track_sensitive (source : source) (* RMS target. *) rmst
  (* Number of samples for computing rms. *)
    window (* Spring coefficient when the sound is going louder. *) kup
  (* Spring coefficient when the sound is going less loud. *)
    kdown threshold gmin gmax =
  let rmsi = Frame.audio_of_seconds window in
  object (self)
    inherit operator ~name:"normalize" [source]

    (** Current squares of RMS. *)
    val mutable rms = 0.

    (** Current number of samples used to compute [rmsl] and [rmsr]. *)
    val mutable rmsc = 0

    (** Volume coefficient. *)
    val mutable v = 1.

    (** Previous volume coefficient. *)
    val mutable vold = 1.

    method gain = v

    (** Last fully computed rms. *)
    val mutable last_rms = 0.

    method rms = last_rms

    method init =
      rms <- 0.;
      rmsc <- 0;
      v <- 1.;
      vold <- 1.

    initializer self#on_wake_up (fun () -> self#init)
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track

    method private normalize buf =
      let b = Content.Audio.get_data (Frame.get buf Frame.Fields.audio) in
      let rmst = rmst () in
      let kup = kup () in
      let kdown = kdown () in
      let threshold = threshold () in
      let gmin = gmin () in
      let gmax = gmax () in
      for i = 0 to source#frame_audio_position - 1 do
        for c = 0 to self#audio_channels - 1 do
          let bc = b.(c) in
          let x = bc.(i) in
          rms <- rms +. (x *. x);
          bc.(i) <-
            x
            *. ((float rmsc *. vold) +. (float (rmsi - rmsc) *. v))
            /. float rmsi
        done;
        rmsc <- rmsc + 1;
        if rmsc >= rmsi then (
          let r = sqrt (rms /. float_of_int (rmsi * self#audio_channels)) in
          last_rms <- r;
          if r > threshold then
            if r *. v > rmst then v <- v +. (kdown *. ((rmst /. r) -. v))
            else v <- v +. (kup *. ((rmst /. r) -. v));
          vold <- v;
          v <- max gmin (min gmax v);
          rms <- 0.;
          rmsc <- 0)
      done;
      Frame.set_data buf Frame.Fields.audio Content.Audio.lift_data b

    method private generate_frame =
      match self#split_frame (source#get_mutable_frame Frame.Fields.audio) with
        | buf, None -> self#normalize buf
        | buf, Some new_track ->
            let buf = self#normalize buf in
            if track_sensitive then self#init;
            Frame.append buf (self#normalize new_track)
  end

let normalize = Lang.add_module "normalize"

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:normalize "old"
    [
      ( "target",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-13.)),
        Some "Desired RMS (dB)." );
      ( "window",
        Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "Duration of the window used to compute the current RMS power \
           (second)." );
      ( "k_up",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.005),
        Some
          "Coefficient when the power must go up (between 0 and 1, slowest to \
           fastest)." );
      ( "k_down",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "Coefficient when the power must go down (between 0 and 1, slowest \
           to fastest)." );
      ( "threshold",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-40.)),
        Some "Minimal RMS for activaing gain control (dB)." );
      ( "gain_min",
        Lang.getter_t Lang.float_t,
        Some (Lang.float (-6.)),
        Some "Minimal gain value (dB)." );
      ( "gain_max",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 6.),
        Some "Maximal gain value (dB)." );
      ( "track_sensitive",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Reset values on every track." );
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`Audio
    ~descr:
      "Normalize the signal. Dynamic normalization of the signal is sometimes \
       the only option, and can make a listening experience much nicer. \
       However, its dynamic aspect implies some limitations which can go as \
       far as creating saturation in some extreme cases. If possible, consider \
       using some track-based normalization techniques such as those based on \
       replay gain. See the documentation for more details. This is the \
       implementation provided in Liquidsoap < 2.0. A new, better and more \
       customizable one is now given in `normalize`."
    ~meth:
      Lang.
        [
          {
            name = "gain";
            scheme = ([], Lang.fun_t [] Lang.float_t);
            descr = "Current amplification coefficient.";
            value = (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#gain));
          };
          {
            name = "rms";
            scheme = ([], Lang.fun_t [] Lang.float_t);
            descr = "Current RMS.";
            value = (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#rms));
          };
        ]
    (fun p ->
      let f v = List.assoc v p in
      let target, window, kup, kdown, threshold, gmin, gmax, src =
        ( Lang.to_float_getter (f "target"),
          Lang.to_float (f "window"),
          Lang.to_float_getter (f "k_up"),
          Lang.to_float_getter (f "k_down"),
          Lang.to_float_getter (f "threshold"),
          Lang.to_float_getter (f "gain_min"),
          Lang.to_float_getter (f "gain_max"),
          Lang.to_source (f "") )
      in
      let track_sensitive = Lang.to_bool (f "track_sensitive") in
      new normalize
        ~track_sensitive src
        (fun () -> Audio.lin_of_dB (target ()))
        window kup kdown
        (fun () -> Audio.lin_of_dB (threshold ()))
        (fun () -> Audio.lin_of_dB (gmin ()))
        (fun () -> Audio.lin_of_dB (gmax ())))
