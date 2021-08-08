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

(* DFT bands *)
type band = {
  band_k : int;
  (* band number *)
  mutable band_x : float;
  (* intensity of the band *)
  band_f : float;
  (* frequency being detected *)
  band_cos : float;
  (* precomputed 2cos(2πk/N) *)
  mutable band_v : float;
  (* current value *)
  mutable band_v' : float; (* previous value *)
}

let key =
  let keys =
    [
      ((697., 1209.), '1');
      ((697., 1336.), '2');
      ((697., 1477.), '3');
      ((697., 1633.), 'A');
      ((770., 1209.), '4');
      ((770., 1336.), '5');
      ((770., 1477.), '6');
      ((770., 1633.), 'B');
      ((852., 1209.), '7');
      ((852., 1336.), '8');
      ((852., 1477.), '9');
      ((852., 1633.), 'C');
      ((941., 1209.), '*');
      ((941., 1336.), '0');
      ((941., 1477.), '#');
      ((941., 1633.), 'D');
    ]
  in
  fun f -> List.assoc f keys

class dtmf ~kind ~duration ~bands ~threshold ~smoothing ~debug callback
  (source : source) =
  let samplerate = float (Lazy.force Frame.audio_rate) in
  let nbands = bands in
  let size = float nbands in
  object (self)
    inherit operator ~name:"dtmf" kind [source] as super

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    val bands =
      let band k =
        {
          band_k = k;
          band_x = 0.;
          band_f = float k /. size *. samplerate;
          band_cos = 2. *. cos (2. *. Float.pi *. float k /. size);
          band_v = 0.;
          band_v' = 0.;
        }
      in
      let band_freq f =
        let b = band (Float.to_int ((f /. samplerate *. size) +. 0.5)) in
        { b with band_f = f }
      in
      List.map band_freq [697.; 770.; 852.; 941.; 1209.; 1336.; 1477.; 1633.]

    val mutable n = nbands

    val mutable state = `None

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
      let debug = debug () in
      let duration = duration () in
      let threshold = threshold () in
      let alpha = min 1. (size /. (samplerate *. smoothing ())) in
      for i = offset to position - 1 do
        let x =
          let x = ref 0. in
          for c = 0 to channels - 1 do
            x := !x +. b.(c).{i}
          done;
          !x /. float channels
        in
        List.iter
          (fun b ->
            let v = x +. (b.band_cos *. b.band_v) -. b.band_v' in
            b.band_v' <- b.band_v;
            b.band_v <- v)
          bands;
        n <- n + 1;
        if n mod nbands = 0 then (
          n <- n - nbands;
          List.iter
            (fun b ->
              (* Square of the value for the DFT band. *)
              let x =
                (b.band_v *. b.band_v) +. (b.band_v' *. b.band_v')
                -. (b.band_cos *. b.band_v *. b.band_v')
              in
              b.band_x <- ((1. -. alpha) *. b.band_x) +. (alpha *. x);
              (* Apparently we need to reset values, otherwise some unexpected
                 band get high values over time. *)
              b.band_v <- 0.;
              b.band_v' <- 0.;
              if debug then (
                let bar x =
                  let len = 20 in
                  let n = Float.to_int (x *. float len /. 20000.) in
                  let n = min len n in
                  String.make n '=' ^ String.make (len - n) ' '
                in
                let bar2 = bar b.band_x in
                let bar = bar x in
                Printf.printf "%02d / %.01f :\t%s %s %.01f\t%.01f\n" b.band_k
                  b.band_f bar bar2 x b.band_x ))
            bands;
          if debug then (
            Printf.printf "\n";
            ( match state with
              | `None -> Printf.printf "No key detected.\n"
              | `Detected (f, t) ->
                  let k = try String.make 1 (key f) with Not_found -> "???" in
                  Printf.printf "Detected key %s for %.03f seconds.\n" k t
              | `Signaled f ->
                  let k = try String.make 1 (key f) with Not_found -> "???" in
                  Printf.printf "Signaled key %s.\n" k );
            print_newline () );
          (* We are looking for bands threshold times higher than the lowest band. *)
          let found =
            let threshold =
              let min =
                List.fold_left (fun m b -> min m b.band_x) infinity bands
              in
              min *. threshold
            in
            List.filter_map
              (fun b -> if b.band_x > threshold then Some b.band_f else None)
              bands
          in
          (* Update the state *)
          match found with
            | [f1; f2] -> (
                let f = (f1, f2) in
                let dt = size /. samplerate in
                match state with
                  | `Detected (f', t) when f' = f ->
                      let t = t +. dt in
                      if t < duration then state <- `Detected (f, t)
                      else (
                        ( try
                            let k = String.make 1 (key f) in
                            (* Printf.printf "Found %s\n%!" k; *)
                            ignore (Lang.apply callback [("", Lang.string k)])
                          with Not_found ->
                            ()
                            (* Printf.printf "Unknown combination (%.01f, %.01f)...\n%!" (fst f) (snd f) *)
                        );
                        state <- `Signaled f )
                  | `Signaled f' when f' = f -> ()
                  | _ -> state <- `Detected (f, dt) )
            | _ -> state <- `None )
      done
  end

let () = Lang.add_module "dtmf"

let () =
  let kind = Lang.audio_pcm in
  let k = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "dtmf.detect"
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.05),
        Some "Duration for detecting a tone." );
      ( "bands",
        Lang.int_t,
        Some (Lang.int 1024),
        Some "Number of frequency bands." );
      ( "threshold",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 100.),
        Some "Threshold for detecting a band." );
      ( "smoothing",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.01),
        Some
          "Smoothing time (in seconds) for band indensity (the higher, the \
           less sensitive we are to local variations, but the more time we \
           take to detect a band)." );
      ( "debug",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Show internal values on standard output in order to fine-tune \
           parameters: band number, band frequency, detected intensity and \
           smoothed intensity." );
      ( "",
        Lang.source_t k,
        None,
        Some "Source on which DTMF tones should be detected" );
      ( "",
        Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t,
        None,
        Some "Function called with detected key as argument." );
    ]
    ~return_t:k ~category:Lang.SoundProcessing ~descr:"Detect DTMF tones."
    (fun p ->
      let duration = List.assoc "duration" p |> Lang.to_float_getter in
      let bands = List.assoc "bands" p |> Lang.to_int in
      let threshold = List.assoc "threshold" p |> Lang.to_float_getter in
      let smoothing = List.assoc "smoothing" p |> Lang.to_float_getter in
      let debug = List.assoc "debug" p |> Lang.to_bool_getter in
      let s = Lang.assoc "" 1 p |> Lang.to_source in
      let callback = Lang.assoc "" 2 p in
      let kind = Source.Kind.of_kind kind in
      ( new dtmf ~kind ~duration ~bands ~threshold ~smoothing ~debug callback s
        :> Source.source ))
