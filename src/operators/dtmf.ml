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
  line_cos : float;
  (* precomputed 2cos(2πk/N) *)
  mutable line_v : float;
  (* current value *)
  mutable line_v' : float;
  (* previous value *)
  mutable line_v'' : float; (* previous previous value *)
}

class dtmf ~kind (source : source) =
  let samplerate = float (Lazy.force Frame.audio_rate) in
  (* Size of the DFT (usually noted N). *)
  let size = 256 in
  object (self)
    inherit operator ~name:"dtmf" kind [source] as super

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    (* TODO: only compute required v *)
    val v =
      List.init size (fun k ->
          {
            line_k = k;
            line_cos = 2. *. cos (2. *. Float.pi *. float k /. float size);
            line_v = 0.;
            line_v' = 0.;
            line_v'' = 0.;
          })

    val mutable n = size

    method wake_up a = super#wake_up a

    (* let channels = self#audio_channels in *)
    (* low <- Array.make channels 0.; *)
    (* high <- Array.make channels 0.; *)
    (* band <- Array.make channels 0.; *)
    (* notch <- Array.make channels 0. *)

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
      (* let channels = self#audio_channels in *)
      for i = offset to position - 1 do
        (* TODO: mean of all channels *)
        let x = b.(0).{i} in
        List.iter
          (fun l ->
            l.line_v'' <- l.line_v';
            l.line_v' <- l.line_v;
            l.line_v <- (l.line_cos *. l.line_v') -. l.line_v'' +. x)
          v;
        n <- n + 1;
        if n mod size = 0 then (
          n <- n - size;
          List.iter
            (fun l ->
              (* square of the value for the DFT line *)
              let v = l.line_v *. l.line_v in
              let v' = l.line_v' *. l.line_v' in
              let x = v +. v' -. (l.line_cos *. v *. v') in
              let f = float l.line_k /. float size *. samplerate in
              Printf.printf "%d / %f : %f\n" l.line_k f x)
            v;
          print_newline () )
      done
  end

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
