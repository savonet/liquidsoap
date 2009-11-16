(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

class biquad ~kind (source:source) filter_type freq fparam db_gain =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let rate = float_of_int (Frame.audio_of_seconds 1.) in
object (self)
  inherit operator kind [source] as super

  (* Coefficients *)
  val mutable coeffs = Array.make 5 0.

  (* I/O shift registries *)
  val mutable xv = Array.make_matrix channels 3 0.
  val mutable yv = Array.make_matrix channels 3 0.

  initializer
    self#log#f 4 "Initializing..." ;
    let a =
      match filter_type with
        (* Peak / Shelf *)
        | Peak
        | Low_shelf
        | High_shelf ->
            self#log#f 4
              "A = 10 ^ (db_gain / 40) = %+.013f"
              (10. ** (db_gain /. 40.)) ;
            10. ** (db_gain /. 40.)
        (* Others *)
        | _ -> 0.
    in
    let w0 = 2. *. pi *. freq /. rate in
    self#log#f 4 "ω0 = 2π f0 / Fs = %+.013f." w0 ;
    let alpha =
      match filter_type with
        | All_pass ->
            self#log#f 4 "BW = %+.013f" fparam ;
            self#log#f 4 "α = sin(w0)*sinh(ln(2)/2 * BW * (w0/sin(w0)))" ;
            (sin w0) *. sinh (fparam *. (w0 /. sin w0) *. ((log 2.) /. 2.))
        (* Shelving *)
        | Low_shelf
        | High_shelf ->
            self#log#f 4 "S = %+.013f" fparam ;
            self#log#f 4 "α = sin(w0)/2 * √ ((A + 1/A) * (1/S - 1) + 2)" ;
            ((sin w0) /. 2.)
             *. sqrt ((a +. 1. /. a) *. ((1. /. fparam) -. 1.) +. 2.)
        (* Q *)
        | _ ->
            self#log#f 4 "Q = %+.013f" fparam ;
            self#log#f 4 "α = sin(w0) / 2Q";
            (sin w0) /. (2. *. fparam)
    in
      self#log#f 4 "α = %+.013f" alpha ;
      let c0 = cos w0 in
        coeffs <-
          (match filter_type with
            | Low_pass ->
              self#log#f 4 "This is a low-pass filter." ;
              let a0 = 1. +. alpha in
                [| (1. -. c0) /. (2. *. a0) ;    (* b0/a0 *)
                   (1. -. c0) /. a0 ;            (* b1/a0 *)
                   (1. -. c0) /. (2. *. a0) ;    (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | High_pass ->
              self#log#f 4 "This is a high-pass filter." ;
              let a0 = 1. +. alpha in
                [| (1. +. c0) /. (2. *. a0) ;    (* b0/a0 *)
                   -. (1. +. c0) /. a0 ;         (* b1/a0 *)
                   (1. +. c0) /. (2. *. a0) ;    (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | Band_pass ->
              self#log#f 4 "This is a band-pass filter." ;
              let a0 = 1. +. alpha in
                [| alpha /. a0 ;                 (* b0/a0 *)
                   0. ;                          (* b1/a0 *)
                   -. alpha /. a0 ;              (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | Notch ->
              self#log#f 4 "This is a notch filter." ;
              let a0 = 1. +. alpha in
                [| 1. /. a0 ;                    (* b0/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* b1/a0 *)
                   1. /. a0 ;                    (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | All_pass ->
              self#log#f 4 "This is an all-pass filter." ;
              let a0 = 1. +. alpha in
                [| (1. -. alpha) /. a0 ;         (* b0/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* b1/a0 *)
                   (1. +. alpha) /. a0 ;         (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. alpha) /. a0 |]        (* a2/a0 *)
            | Peak ->
              self#log#f 4 "This is a peak filter." ;
              let a0 = 1. +. (alpha /. a) in
                [| (1. +. (alpha *. a)) /. a0 ;  (* b0/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* b1/a0 *)
                   (1. -. (alpha *. a)) /. a0 ;  (* b2/a0 *)
                   (-. 2. *. c0) /. a0 ;         (* a1/a0 *)
                   (1. -. (alpha /. a)) /. a0 |] (* a2/a0 *)
            | Low_shelf ->
              self#log#f 4 "This is a low-shelf filter." ;
              let s = 2. *. (sqrt a) *. alpha in
              let a0 = (a +. 1.) +. (a -. 1.) *. c0 +. s in
                [| (a *. ((a +. 1.) -. (a -. 1.) *. c0 +. s)) /. a0 ;
                        (* b0/a0 *)
                   (2. *. a *. ((a -. 1.) -. (a +. 1.) *. c0)) /. a0 ;
                        (* b1/a0 *)
                   (a *. ((a +. 1.) -. (a -. 1.) *. c0 -. s)) /. a0 ;
                        (* b2/a0 *)
                   (-. 2. *. ((a -. 1.) +. (a +. 1.) *. c0)) /. a0 ;
                        (* a1/a0 *)
                   ((a +. 1.) +. (a -. 1.) *. c0 -. s) /. a0
                        (* a2/a0 *)
                |]
            | High_shelf ->
              self#log#f 4 "This is an high-shelf filter." ;
              let s = 2. *. (sqrt a) *. alpha in
              let a0 = (a +. 1.) -. (a -. 1.) *. c0 +. s in
                [| (a *. ((a +. 1.) +. (a -. 1.) *. c0 +. s)) /. a0 ;
                       (* b0/a0 *)
                   (-. 2. *. a *. ((a -. 1.) +. (a +. 1.) *. c0)) /. a0 ;
                       (* b1/a0 *)
                   (a *. ((a +. 1.) +. (a -. 1.) *. c0 -. s)) /. a0 ;
                       (* b2/a0 *)
                   (2. *. ((a -. 1.) -. (a +. 1.) *. c0)) /. a0 ;
                       (* a1/a0 *)
                   ((a +. 1.) -. (a -. 1.) *. c0 -. s) /. a0
                       (* a2/a0 *)
                |]) ;
        self#log#f 4 "Coefficients:" ;
        self#log#f 4 "%s"
          (String.concat "\n"
             (Array.to_list
                (Array.mapi
                   (fun i a -> Printf.sprintf "%d: %+.013f." i a)
                   coeffs))) ;
        self#log#f 4 "Initialization done."

  (* Digital filter based on "Cookbook formulae for audio EQ biquad filter
     coefficients" by Robert Bristow-Johnson <rbj@audioimagination.com>.
     URL: http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt *)

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method private get_frame buf =
    let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf offset in
      let shift a = for i = 0 to Array.length a - 2 do a.(i) <- a.(i+1) done in
        for c = 0 to channels - 1 do
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
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.lowshelf"
    [
      "frequency", Lang.float_t, None, Some ("Corner frequency") ;
      "slope", Lang.float_t, Some (Lang.float 1.),
        Some "Shelf slope (dB/octave)" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"Low shelf biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "slope"),
         Lang.to_source (f "") in
         new biquad ~kind src Low_shelf freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.highshelf"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "slope", Lang.float_t, Some (Lang.float 1.),
        Some "Shelf slope (in dB/octave)" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"High shelf biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "slope"),
         Lang.to_source (f "") in
         new biquad ~kind src High_shelf freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.low"
    [
      "frequency", Lang.float_t, None, Some ("Corner frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"Low pass biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_source (f "") in
         new biquad ~kind src Low_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.high"
    [
      "frequency", Lang.float_t, None, Some ("Corner frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"High pass biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_source (f "") in
         new biquad ~kind src High_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.bandpass"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"Band pass biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_source (f "") in
         new biquad ~kind src Band_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.allpass"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "bandwidth", Lang.float_t, Some (Lang.float (1./.3.)),
        Some "Bandwidth (in octaves)" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"All pass biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "bandwidth"),
         Lang.to_source (f "") in
         new biquad ~kind src All_pass freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.notch"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"Band pass biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_source (f "") in
         new biquad ~kind src Notch freq param 0.)

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "filter.iir.eq.peak"
    [
      "frequency", Lang.float_t, None, Some ("Center frequency") ;
      "q", Lang.float_t, Some (Lang.float 1.), Some "Q" ;
      "gain", Lang.float_t, Some (Lang.float 1.), Some "Gain (in dB)" ;
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.SoundProcessing
    ~descr:"Peak EQ biquad filter."
    (fun p kind ->
       let f v = List.assoc v p in
       let freq, param, gain, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_float (f "gain"),
         Lang.to_source (f "") in
         new biquad ~kind src Peak freq param gain)
