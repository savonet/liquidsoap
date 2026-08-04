(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

class rms ~tau source =
  let samplerate = float (Lazy.force Frame.audio_rate) in
  object (self)
    inherit operator [source] ~name:"rms"
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method effective_source = source#effective_source
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    val mutable rms = 0.
    method rms = sqrt rms

    method private generate_frame =
      let chans = self#audio_channels in
      let a = 1. -. exp (-1. /. (tau () *. samplerate)) in
      let frame = source#get_frame in
      let position = AFrame.position frame in
      let buf = AFrame.pcm frame in
      for i = 0 to position - 1 do
        let r = ref 0. in
        for c = 0 to chans - 1 do
          let x = buf.(c).(i) in
          r := !r +. (x *. x)
        done;
        let r = !r /. float chans in
        rms <- ((1. -. a) *. rms) +. (a *. r)
      done;
      frame
  end

let _ =
  let return_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Window_op.rms "smooth" ~category:`Visualization
    ~meth:
      Lang.
        [
          {
            name = "rms";
            scheme = ([], Lang.fun_t [] Lang.float_t);
            descr = "Current value for the RMS.";
            value = (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#rms));
          };
        ]
    ~return_t
    ~descr:
      "Compute the current RMS for the source, this varies more smoothly that \
       `rms` and is updated more frequently. Returns the source with a method \
       `rms`."
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.5),
        Some
          "Duration of the window in seconds (more precisely, this is the time \
           constant of the low-pass filter)." );
      ("", Lang.source_t return_t, None, None);
    ]
    (fun p ->
      let duration = List.assoc "duration" p |> Lang.to_float_getter in
      let src = List.assoc "" p |> Lang.to_source in
      new rms ~tau:duration src)
