(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source

let pi = 3.14159265358979323846

type filter_type = All_pass | Band_pass | High_pass | Low_pass
                   | Notch | Peak | High_shelf | Low_shelf

class biquad (source:source) filter_type freq fparam db_gain debug =
object (self)
  inherit operator [source] as super
  
  (* Coefficients *)
  val mutable coeffs = Array.make 5 0.
  
  (* I/O shift registries *)
  val mutable xv = Array.make_matrix (Fmt.channels ()) 3 0.
  val mutable yv = Array.make_matrix (Fmt.channels ()) 3 0.
  
  initializer
    if debug then
      (
        Printf.printf "================== BEG INIT FILTER %s ==================\n" self#id
      ) ;
    let a =
      (match filter_type with
        (* Peak / Shelf *)
        | Peak
        | Low_shelf
        | High_shelf ->
          (
            if debug then
              (
                Printf.printf "A = 10 ^ (db_gain / 40) = %+.013f\n" (10. ** (db_gain /. 40.))
              ) ;
            10. ** (db_gain /. 40.)
          )
        (* Others *)
        | _ -> 0.)
    in
    let w0 = 2. *. pi *. freq /. float_of_int (Fmt.samples_per_second ()) in
    if debug then
      (
        Printf.printf "ω0 = 2π f0 / Fs = %+.013f.\n" w0
      ) ;
    let alpha =
      (match filter_type with
        (* Bandwidth *)
(*        | Notch  ----> Now we use Q for them 3 too
        | Peak
        | Band_pass *)
        | All_pass ->
          (
            if debug then
              (
                Printf.printf "BW = %+.013f\n" fparam ;
                Printf.printf "α = sin(w0)*sinh(ln(2)/2 * BW * (w0/sin(w0)))\n"
              ) ;
            (sin w0) *. sinh (fparam *. (w0 /. sin w0) *. ((log 2.) /. 2.))
          )
        (* Shelving *)
        | Low_shelf
        | High_shelf ->
          (
            if debug then
              (
                Printf.printf "S = %+.013f\n" fparam ;
                Printf.printf "α = sin(w0)/2 * √ ((A + 1/A) * (1/S - 1) + 2)\n"
              ) ;
            ((sin w0) /. 2.)
              *. sqrt ((a +. 1. /. a) *. ((1. /. fparam) -. 1.) +. 2.)
          )
        (* Q *)
        | _ ->
          (
            if debug then
              (
                Printf.printf "Q = %+.013f\n" fparam ;
                Printf.printf "α = sin(w0) / 2Q\n";
              ) ;
            (sin w0) /. (2. *. fparam))
          )
    in
      if debug then
        (
          Printf.printf "α = %+.013f\n" alpha
        ) ;
      let c0 = cos w0 in
        coeffs <-
          (match filter_type with
            | Low_pass ->
              if debug then (Printf.printf "This is a low-pass filter.\n") ;
              let a0 = 1. +. alpha in
                [| (1. -. c0) /. (2. *. a0) ;    (* b0/a0 *)
                   (1. -. c0) /. a0 ;            (* b1/a0 *)
                   (1. -. c0) /. (2. *. a0) ;    (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | High_pass ->
              if debug then (Printf.printf "This is a high-pass filter.\n") ;
              let a0 = 1. +. alpha in
                [| (1. +. c0) /. (2. *. a0) ;    (* b0/a0 *)
                   -. (1. +. c0) /. a0 ;         (* b1/a0 *)
                   (1. +. c0) /. (2. *. a0) ;    (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | Band_pass ->
              if debug then (Printf.printf "This is a band-pass filter.\n") ;
              let a0 = 1. +. alpha in
                [| alpha /. a0 ;                 (* b0/a0 *)
                   0. ;                          (* b1/a0 *)
                   -. alpha /. a0 ;              (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | Notch ->
              if debug then (Printf.printf "This is a notch filter.\n") ;
              let a0 = 1. +. alpha in
                [| 1. /. a0 ;                    (* b0/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* b1/a0 *)
                   1. /. a0 ;                    (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | All_pass ->
              if debug then (Printf.printf "This is an all-pass filter.\n") ;
              let a0 = 1. +. alpha in
                [| (1. -. alpha) /. a0 ;         (* b0/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* b1/a0 *)
                   (1. +. alpha) /. a0 ;         (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | Peak ->
              if debug then (Printf.printf "This is a peak filter.\n") ;
              let a0 = 1. +. (alpha /. a) in
                [| (1. +. (alpha *. a)) /. a0 ;  (* b0/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* b1/a0 *)
                   (1. -. (alpha *. a)) /. a0 ;  (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. (alpha /. a)) /. a0 |] (* a2/a0 *)
            | Low_shelf ->
              if debug then (Printf.printf "This is a low-shelf filter.\n") ;
              let s = 2. *. (sqrt a) *. alpha in
              let a0 = (a +. 1.) +. (a -. 1.) *. c0 +. s in
                [| (a *. ((a +. 1.) -. (a -. 1.) *. c0 +. s)) /. a0 ;     (* b0/a0 *)
                   (2. *. a *. ((a -. 1.) -. (a +. 1.) *. c0)) /. a0 ;    (* b1/a0 *)
                   (a *. ((a +. 1.) -. (a -. 1.) *. c0 -. s)) /. a0 ;     (* b2/a0 *)
                   (-. 2. *. ((a -. 1.) +. (a +. 1.) *. c0)) /. a0 ;      (* a1/a0 *)
                   ((a +. 1.) +. (a -. 1.) *. c0 -. s) /. a0 |]           (* a2/a0 *)
            | High_shelf ->
              if debug then (Printf.printf "This is an high-shelf filter.\n") ;
              let s = 2. *. (sqrt a) *. alpha in
              let a0 = (a +. 1.) -. (a -. 1.) *. c0 +. s in
                [| (a *. ((a +. 1.) +. (a -. 1.) *. c0 +. s)) /. a0 ;     (* b0/a0 *)
                   (-. 2. *. a *. ((a -. 1.) +. (a +. 1.) *. c0)) /. a0 ; (* b1/a0 *)
                   (a *. ((a +. 1.) +. (a -. 1.) *. c0 -. s)) /. a0 ;     (* b2/a0 *)
                   (2. *. ((a -. 1.) -. (a +. 1.) *. c0)) /. a0 ;         (* a1/a0 *)
                   ((a +. 1.) -. (a -. 1.) *. c0 -. s) /. a0 |]           (* a2/a0 *)) ;
        if debug then
          (
            Printf.printf "Coefficients:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f." i a) coeffs)))) ;
            Printf.printf "================== END INIT FILTER %s ==================\n" self#id
          )
  
  (* Digital filter based on "Cookbook formulae for audio EQ biquad filter
     coefficients" by Robert Bristow-Johnson <rbj@audioimagination.com>.
     URL: http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt *)

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.get_float_pcm buf in
      let shift a = for i = 0 to Array.length a - 2 do a.(i) <- a.(i+1) done in
        for c = 0 to (Fmt.channels ()) - 1 do
          for i = offset to AFrame.position buf - 1 do
            shift xv.(c) ;
            xv.(c).(2) <- b.(c).(i) ;
            shift yv.(c) ;
            let insert =
              (coeffs.(0) *. xv.(c).(2))
                +. (coeffs.(1) *. xv.(c).(1))
                +. (coeffs.(2) *. xv.(c).(0))
                -. (coeffs.(3) *. yv.(c).(1))
                -. (coeffs.(4) *. yv.(c).(0))
            in
              yv.(c).(2) <- insert ;
              b.(c).(i) <- insert ;
          done;
        done;
end

let () =
  Lang.add_operator "filter.iir.eq.lowshelf"
    [
      "frequency", Lang.float_t, None, Some ("Corner frequency") ;
      "slope", Lang.float_t, Some (Lang.float 1.),
        Some "Shelf slope (dB/octave)" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Low shelf biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "slope"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src Low_shelf freq param 0. debug):>source)
    )

let () =
  Lang.add_operator "filter.iir.eq.highshelf"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "slope", Lang.float_t, Some (Lang.float 1.),
        Some "Shelf slope (in dB/octave)" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"High shelf biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "slope"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src High_shelf freq param 0. debug):>source)
    )

let () =
  Lang.add_operator "filter.iir.eq.low"
    [
      "frequency", Lang.float_t, None, Some ("Corner frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Low pass biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src Low_pass freq param 0. debug):>source)
    )

let () =
  Lang.add_operator "filter.iir.eq.high"
    [
      "frequency", Lang.float_t, None, Some ("Corner frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"High pass biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src High_pass freq param 0. debug):>source)
    )

let () =
  Lang.add_operator "filter.iir.eq.bandpass"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Band pass biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src Band_pass freq param 0. debug):>source)
    )

let () =
  Lang.add_operator "filter.iir.eq.allpass"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "bandwidth", Lang.float_t, Some (Lang.float (1./.3.)),
        Some "Bandwidth (in octaves)" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"All pass biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "bandwidth"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src All_pass freq param 0. debug):>source)
    )

let () =
  Lang.add_operator "filter.iir.eq.notch"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Band pass biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src Notch freq param 0. debug):>source)
    )

let () =
  Lang.add_operator "filter.iir.eq.peak"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "gain", Lang.float_t, Some (Lang.float 1.), Some "Gain (in dB)" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Peak EQ biquad filter."
    (fun p ->
       let f v = List.assoc v p in
       let freq, param, gain, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_float (f "gain"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new biquad src Peak freq param gain debug):>source)
    )

