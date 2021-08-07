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

(* DFT lines *)
type line = {
  line_k : int;
  line_f : float;
  (* frequency being detected *)
  line_cos : float;
  (* precomputed 2cos(2πk/N) *)
  mutable line_v : float;
  (* current value *)
  mutable line_v' : float; (* previous value *)
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
      let line k =
        {
          line_k = k;
          line_f = float k /. float size *. samplerate;
          line_cos = 2. *. cos (2. *. Float.pi *. float k /. float size);
          line_v = 0.;
          line_v' = 0.;
        }
      in
      let line_freq f =
        let l = line (Float.to_int ((f /. samplerate *. float size) +. 0.5)) in
        { l with line_f = f }
      in
      List.map line_freq [697.; 770.; 852.; 941.; 1209.; 1336.; 1477.; 1633.]

    (* List.init (size / 3) line *)
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
            let v = x +. (l.line_cos *. l.line_v) -. l.line_v' in
            l.line_v' <- l.line_v;
            l.line_v <- v)
          v;
        n <- n + 1;
        if n mod size = 0 then (
          n <- n - size;
          List.iter
            (fun l ->
              (* square of the value for the DFT line *)
              let x =
                (l.line_v *. l.line_v) +. (l.line_v' *. l.line_v')
                -. (l.line_cos *. l.line_v *. l.line_v')
              in
              Printf.printf "%d / %f : %f\n" l.line_k l.line_f x)
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
