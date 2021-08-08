(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

let debug = true

(* DFT bands *)
type band = {
  band_k : int;
  band_f : float;
  (* frequency being detected *)
  band_cos : float;
  (* precomputed 2cos(2πk/N) *)
  mutable band_v : float;
  (* current value *)
  mutable band_v' : float; (* previous value *)
}

class dtmf ~kind (source : source) =
  let samplerate = float (Lazy.force Frame.audio_rate) in
  (* Size of the DFT (usually noted N). *)
  let size = 512 in
  object (self)
    inherit operator ~name:"dtmf" kind [source] as super

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    val v =
      let band k =
        {
          band_k = k;
          band_f = float k /. float size *. samplerate;
          band_cos = 2. *. cos (2. *. Float.pi *. float k /. float size);
          band_v = 0.;
          band_v' = 0.;
        }
      in
      let band_freq f =
        let l = band (Float.to_int ((f /. samplerate *. float size) +. 0.5)) in
        { l with band_f = f }
      in
      List.map band_freq [697.; 770.; 852.; 941.; 1209.; 1336.; 1477.; 1633.]

    (* List.init (size / 3) band *)
    val mutable n = size

    method wake_up a = super#wake_up a

    (* See
       https://en.wikipedia.org/wiki/Goertzel_algorithm
       https://web.archive.org/web/20180628024641/http://en.dsplib.org/content/goertzel/goertzel.html
       https://www.ti.com/lit/pdf/spra096
    *)
    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.pcm buf in
      let position = AFrame.position buf in
      let channels = self#audio_channels in
      for i = offset to position - 1 do
        let x =
          let x = ref 0. in
          for c = 0 to channels - 1 do
            x := !x +. b.(c).{i}
          done;
          !x /. float channels
        in
        List.iter
          (fun l ->
            let v = x +. (l.band_cos *. l.band_v) -. l.band_v' in
            l.band_v' <- l.band_v;
            l.band_v <- v)
          v;
        n <- n + 1;
        if n mod size = 0 then (
          n <- n - size;
          List.iter
            (fun l ->
              (* square of the value for the DFT band *)
              let x =
                (l.band_v *. l.band_v) +. (l.band_v' *. l.band_v')
                -. (l.band_cos *. l.band_v *. l.band_v')
              in
              if debug then (
                let bar =
                  let len = 20 in
                  let n = Float.to_int (x *. float len /. 20000.) in
                  let n = min len n in
                  String.make n '=' ^ String.make (len - n) ' '
                in
                Printf.printf "%d / %f :\t%s %f\n" l.band_k l.band_f bar x ))
            v;
          print_newline () )
      done
  end

let () = Lang.add_module "dtmf"

let () =
  let kind = Lang.audio_pcm in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "dtmf.detect"
    [("", Lang.source_t k, None, None)]
    ~return_t:k ~category:Lang.SoundProcessing ~descr:"Detect DTMF tones."
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      let kind = Source.Kind.of_kind kind in
      (new dtmf ~kind s :> Source.source))
