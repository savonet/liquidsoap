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

(* See
   https://en.wikipedia.org/wiki/Goertzel_algorithm
   https://web.archive.org/web/20180628024641/http://en.dsplib.org/content/goertzel/goertzel.html
   https://www.ti.com/lit/pdf/spra096
*)

(** DFT bands. *)
module Band = struct
  (** A band. *)
  type t = {
    band_k : int;  (** band number *)
    mutable band_x : float;  (** intensity of the band *)
    band_f : float;  (** frequency being detected *)
    band_cos : float;  (** precomputed 2cos(2πk/N) *)
    mutable band_v : float;  (** current value *)
    mutable band_v' : float;  (** previous value *)
  }

  let frequency b = b.band_f
  let intensity b = b.band_x

  let to_string ~size ~samplerate b =
    let size = float size in
    Printf.sprintf "band %d at %.02fHz (detecting between %.02fHz and %0.02fHz)"
      b.band_k b.band_f
      (float b.band_k /. size *. samplerate)
      (float (b.band_k + 1) /. size *. samplerate)

  (** Create the band of given number. Size is the total number of bands and
      samplerate is the expected samplerate. *)
  let create ~size ~samplerate k =
    let size = float size in
    {
      band_k = k;
      band_x = 0.;
      band_f = float k /. size *. samplerate;
      band_cos = 2. *. cos (2. *. Float.pi *. float k /. size);
      band_v = 0.;
      band_v' = 0.;
    }

  (** Create band of given frequency. *)
  let make ~size ~samplerate f =
    let b =
      create ~size ~samplerate
        (Float.to_int ((f /. samplerate *. float size) +. 0.5))
    in
    { b with band_f = f }

  (** Feed a band with a sample. *)
  let feed b x =
    let v = x +. (b.band_cos *. b.band_v) -. b.band_v' in
    b.band_v' <- b.band_v;
    b.band_v <- v

  (** Update the value of the band. This function should be called every size
      samples. *)
  let update ?(debug = false) ~alpha b =
    (* Square of the value for the DFT band. *)
    let x =
      (b.band_v *. b.band_v) +. (b.band_v' *. b.band_v')
      -. (b.band_cos *. b.band_v *. b.band_v')
    in
    let x = sqrt x in
    b.band_x <- ((1. -. alpha) *. b.band_x) +. (alpha *. x);
    (* Apparently we need to reset values, otherwise some unexpected bands get
       high values over time. *)
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
      Printf.printf "%02d / %.01f :\t%s %s %.01f\t%.01f\n" b.band_k b.band_f bar
        bar2 x b.band_x)
end

module Bands = struct
  type t = Band.t list

  let to_string ~size ~samplerate (bands : t) =
    List.map (Band.to_string ~size ~samplerate) bands |> String.concat ", "

  let make ~size ~samplerate freqs : t =
    List.map (Band.make ~size ~samplerate) freqs

  let update ?debug ~alpha (bands : t) =
    List.iter (fun b -> Band.update ?debug ~alpha b) bands;
    if debug = Some true then Printf.printf "%!"

  let feed (bands : t) x = List.iter (fun b -> Band.feed b x) bands

  let detect (bands : t) threshold =
    List.filter_map
      (fun b ->
        if Band.intensity b > threshold then Some (Band.frequency b) else None)
      bands
end

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

class dtmf ~duration ~bands ~threshold ~smoothing ~debug callback
  (source : source) =
  let samplerate = float (Lazy.force Frame.audio_rate) in
  let nbands = bands in
  let size = float nbands in
  object (self)
    inherit operator ~name:"dtmf" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    val bands =
      Bands.make ~size:nbands ~samplerate
        [697.; 770.; 852.; 941.; 1209.; 1336.; 1477.; 1633.]

    val mutable n = nbands
    val mutable state = `None

    method private generate_frame =
      let b =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let position = source#frame_audio_position in
      let channels = self#audio_channels in
      let debug = debug () in
      let duration = duration () in
      let threshold = threshold () in
      let alpha = min 1. (size /. (samplerate *. smoothing ())) in
      for i = 0 to position - 1 do
        let x =
          let x = ref 0. in
          for c = 0 to channels - 1 do
            x := !x +. b.(c).(i)
          done;
          !x /. float channels
        in
        Bands.feed bands x;
        n <- n + 1;
        if n mod nbands = 0 then (
          n <- n - nbands;
          Bands.update ~debug ~alpha bands;
          ((* Find relevant bands. *)
           let found = Bands.detect bands threshold in
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
                         (try
                            let k = String.make 1 (key f) in
                            (* Printf.printf "Found %s\n%!" k; *)
                            ignore (Lang.apply callback [("", Lang.string k)])
                          with Not_found ->
                            ()
                            (* Printf.printf "Unknown combination (%.01f, %.01f)...\n%!" (fst f) (snd f) *));
                         state <- `Signaled f)
                   | `Signaled f' when f' = f -> ()
                   | _ -> state <- `Detected (f, dt))
             | _ -> state <- `None);
          if debug then (
            Printf.printf "\n";
            (match state with
              | `None -> Printf.printf "No key detected.\n"
              | `Detected (f, t) ->
                  let k = try String.make 1 (key f) with Not_found -> "???" in
                  Printf.printf "Detected key %s for %.03f seconds.\n" k t
              | `Signaled f ->
                  let k = try String.make 1 (key f) with Not_found -> "???" in
                  Printf.printf "Signaled key %s.\n" k);
            print_newline ()))
      done;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

let dtmf = Lang.add_module "dtmf"

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:dtmf "detect"
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
        Some (Lang.float 50.),
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
        Lang.source_t frame_t,
        None,
        Some "Source on which DTMF tones should be detected." );
      ( "",
        Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t,
        None,
        Some "Function called with detected key as argument." );
    ]
    ~return_t:frame_t ~category:`Audio ~descr:"Detect DTMF tones."
    (fun p ->
      let duration = List.assoc "duration" p |> Lang.to_float_getter in
      let bands = List.assoc "bands" p |> Lang.to_int in
      let threshold = List.assoc "threshold" p |> Lang.to_float_getter in
      let smoothing = List.assoc "smoothing" p |> Lang.to_float_getter in
      let debug = List.assoc "debug" p |> Lang.to_bool_getter in
      let s = Lang.assoc "" 1 p |> Lang.to_source in
      let callback = Lang.assoc "" 2 p in
      (new dtmf ~duration ~bands ~threshold ~smoothing ~debug callback s
        :> Source.source))

class detect ~duration ~bands ~threshold ~smoothing ~debug ~frequencies callback
  (source : source) =
  let samplerate = float (Lazy.force Frame.audio_rate) in
  let nbands = bands in
  let size = float nbands in
  object (self)
    inherit operator ~name:"dtmf.detect" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    val bands = Bands.make ~size:nbands ~samplerate frequencies
    val mutable n = nbands
    val mutable detected = []
    val mutable signaled = []

    initializer
      self#log#info "Listening on the following bands: %s"
        (Bands.to_string ~size:nbands ~samplerate bands)

    method private generate_frame =
      let b =
        Content.Audio.get_data (Frame.get source#get_frame Frame.Fields.audio)
      in
      let position = source#frame_audio_position in
      let channels = self#audio_channels in
      let debug = debug () in
      let duration = duration () in
      let threshold = threshold () in
      let alpha = min 1. (size /. (samplerate *. smoothing ())) in
      for i = 0 to position - 1 do
        let x =
          let x = ref 0. in
          for c = 0 to channels - 1 do
            x := !x +. b.(c).(i)
          done;
          !x /. float channels
        in
        Bands.feed bands x;
        n <- n + 1;
        if n mod nbands = 0 then (
          n <- n - nbands;
          Bands.update ~debug ~alpha bands;
          let found = Bands.detect bands threshold in
          (* Forget about non-detected bands. *)
          detected <- List.filter (fun (f, _) -> List.mem f found) detected;
          signaled <- List.filter (fun f -> List.mem f found) signaled;
          (* Consider not already signaled bands. *)
          let found = List.filter (fun f -> not (List.mem f signaled)) found in
          let dt = size /. samplerate in
          List.iter
            (fun f ->
              let t =
                try List.assoc f detected
                with Not_found ->
                  let t = ref 0. in
                  detected <- (f, t) :: detected;
                  t
              in
              t := !t +. dt;
              if !t >= duration then (
                ignore (Lang.apply callback [("", Lang.float f)]);
                detected <- List.remove_assoc f detected;
                signaled <- f :: signaled))
            found)
      done;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data b
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Audio_gen.sine "detect"
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.5),
        Some "Duration for detecting a tone." );
      ( "bands",
        Lang.int_t,
        Some (Lang.int 1024),
        Some "Number of frequency bands." );
      ( "threshold",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 50.),
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
      ("", Lang.list_t Lang.float_t, None, Some "List of frequencies to detect.");
      ( "",
        Lang.source_t frame_t,
        None,
        Some "Source on which sines should be detected." );
      ( "",
        Lang.fun_t [(false, "", Lang.float_t)] Lang.unit_t,
        None,
        Some "Function called with detected frequency as argument." );
    ]
    ~return_t:frame_t ~category:`Audio ~descr:"Detect sine waves."
    (fun p ->
      let duration = List.assoc "duration" p |> Lang.to_float_getter in
      let bands = List.assoc "bands" p |> Lang.to_int in
      let threshold = List.assoc "threshold" p |> Lang.to_float_getter in
      let smoothing = List.assoc "smoothing" p |> Lang.to_float_getter in
      let debug = List.assoc "debug" p |> Lang.to_bool_getter in
      let frequencies =
        Lang.assoc "" 1 p |> Lang.to_list |> List.map Lang.to_float
      in
      let s = Lang.assoc "" 2 p |> Lang.to_source in
      let callback = Lang.assoc "" 3 p in
      (new detect
         ~duration ~bands ~threshold ~smoothing ~debug ~frequencies callback s
        :> Source.source))
