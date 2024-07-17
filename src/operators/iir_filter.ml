(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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
open Complex

let pi = 3.14159265358979323846

type filter_type = Band_stop | Band_pass | High_pass | Low_pass | All_pass
type filter_family = Butterworth | Resonator

class iir (source:source) filter_family filter_type order
  freq1 freq2 qfactor debug =
object (self)
  inherit operator [source] as super
  
  (* Params *)
  val raw_alpha1 = freq1 /. float_of_int (Fmt.samples_per_second ())
  val raw_alpha2 = freq2 /. float_of_int (Fmt.samples_per_second ())
  val warped_alpha1 = tan(pi *. freq1
                          /. float_of_int (Fmt.samples_per_second ()))
                        /. pi
  val warped_alpha2 = tan(pi *. freq2
                          /. float_of_int (Fmt.samples_per_second ()))
                        /. pi
  val mutable gain = 0.
  
  (* Used for computation *)
  val mutable polemask = 0
  val mutable splane_poles = [||]
  val mutable splane_numpoles = 0
  val mutable splane_zeros = [||]
  val mutable splane_numzeros = 0
  val mutable zplane_poles = [||]
  val mutable zplane_numpoles = 0
  val mutable zplane_zeros = [||]
  val mutable zplane_numzeros = 0
  val mutable topcoeffs = [||]
  val mutable botcoeffs = [||]
  val mutable dc_gain = {re = 0. ; im = 0.}
  val mutable fc_gain = {re = 0. ; im = 0.}
  val mutable hf_gain = {re = 0. ; im = 0.}
  
  (* Coefficients *)
  val mutable xcoeffs = [||]
  val mutable ycoeffs = [||]
  
  (* I/O shift registries *)
  val mutable xv = [||]
  val mutable yv = [||]
  
  initializer
    if debug then
      (
        Printf.printf "================== BEG INIT FILTER %s ==================\n" self#id ;
        Printf.printf "Alpha 1: %.013f (warped: %.013f)\n" raw_alpha1 warped_alpha1 ;
        Printf.printf "Alpha 2: %.013f (warped: %.013f)\n" raw_alpha2 warped_alpha2 ;
        Printf.printf "Q: %.013f\n" qfactor
      ) ;
    let cor a = {re = a ; im = 0.}
      and ( +~ ), ( -~ ), ( *~ ), ( /~ ) =
        Complex.add, Complex.sub, Complex.mul, Complex.div
    in
    let multin w s n c =
      (* multiply factor (z-w) into coeffs *)
      let w = (cor 0. -~ w)
      in
        for i = 0 to n - 1 do
          c.(n - i) <- (w *~ c.(n - i)) +~ c.(n - i - 1) ;
        done;
        c.(0) <- w *~ c.(0) ;
    in
    let expand z n =
      let c = Array.append [|{re = 1. ; im = 0. }|]
              (Array.make n {re = 0. ; im = 0. })
      in
        for i = 0 to n - 1 do
          multin z.(i) i n c ;
        done;
        (* check that computed coeffs of z^k are all real *)
        for i = 0 to n do
          if (abs_float(c.(i).im) > 1e-10) then
            (* coeff of z^i is not real; poles/zeros are not complex conj. *)
            assert false ;
        done;
        c
    in
    let eval c n z =
      (* evaluate polynomial in z, substituting for z *)
      Array.fold_right (fun a b -> Complex.add (Complex.mul b z) a)
        c {re = 0. ; im = 0.}
    in
    let evaluate t nz b np z =
      (* evaluate response, substituting for z *)
      Complex.div (eval t nz z) (eval b np z)
    in
      (match filter_family with
        | Butterworth ->
            (* Compute S-plane poles (Butterworth) *)
            if debug then
              Printf.printf "This is a Butterworth filter.\n" ;
            let choosepole z =
              if (z.re < 0.) then
                (
                  if debug then
                    Printf.printf "z = %+.013f %+.013f i.\n" z.re z.im ;
                  if (polemask mod 2 == 0) then
                    (
                      splane_poles <- Array.append splane_poles [|z|] ;
                      splane_numpoles <- splane_numpoles + 1
                    ) ;
                  polemask <- polemask lsl 1
                ) ;
            in
              if debug then
                Printf.printf "Theta:\n" ;
              for i = 0 to (2 * order) - 1 do
                let theta =
                  match (order mod 2) with
                    | 1 -> (float_of_int i *. pi) /. (float_of_int order)
                    | 0 ->
                        ((float_of_int i +. 0.5) *. pi) /. (float_of_int order)
                    | _ -> assert false
                in
                  if debug then
                    Printf.printf "%d: %+.013f --> %+.013f %+.013f i.\n" i theta (cos theta) (sin theta) ;
                  choosepole ({re = cos(theta) ; im = sin(theta)})
              done;
            (* Normalize *)
            let w1 = cor (2. *. pi *. warped_alpha1)
              and w2 = cor (2. *. pi *. warped_alpha2)
            in
            ( match filter_type with
                | Band_stop ->
                    (* Band-stop filter *)
                    if debug then
                      Printf.printf "This is a band-stop filter.\n" ;
                    let w0 = sqrt (w1 *~ w2)
                      and bw = w2 -~ w1
                    in
                      for i = 0 to splane_numpoles - 1 do
                        let hba = (cor 0.5) *~ (bw /~ splane_poles.(i)) in
                        let t = sqrt (cor 1. -~
                          (Complex.pow (w0 /~ hba) (cor 2.)))
                        in
                          splane_poles.(i) <- hba *~ (cor 1. +~ t) ;
                          splane_poles <- Array.append splane_poles
                                                       [|hba *~ (cor 1. -~ t)|];
                      done;
                      for i = 0 to splane_numpoles - 1 do
                        (* also N zeros at (0,0) *)
                        splane_zeros <- Array.append splane_zeros
                          [|{re = 0. ; im = w0.re} ;
                            {re = 0. ; im = -. w0.re}|] ;
                      done;
                      splane_numpoles <- splane_numpoles * 2 ;
                      splane_numzeros <- splane_numpoles ;
                      xv <- Array.make_matrix (Fmt.channels ())
                        (order * 2 + 1) 0. ;
                      yv <- Array.make_matrix (Fmt.channels ())
                        (order * 2 + 1) 0.
                | Band_pass ->
                    (* Band-pass filter *)
                    if debug then
                      Printf.printf "This is a band-pass filter.\n" ;
                    let w0 = sqrt (w1 *~ w2)
                      and bw = w2 -~ w1
                    in
                      for i = 0 to splane_numpoles - 1 do
                        let hba = (cor 0.5) *~ (splane_poles.(i) *~ bw) in
                        let t = sqrt (cor 1. -~
                          (Complex.pow (w0 /~ hba) (cor 2.)))
                        in
                          splane_poles.(i) <- hba *~ (cor 1. +~ t) ;
                          splane_poles <- Array.append splane_poles
                                                       [|hba *~ (cor 1. -~ t)|];
                      done;
                      for i = 0 to splane_numpoles - 1 do
                        (* also N zeros at (0,0) *)
                        splane_zeros <- Array.append splane_zeros [|cor 0.|] ;
                      done;
                      splane_numzeros <- splane_numpoles ;
                      splane_numpoles <- splane_numpoles * 2 ;
                      xv <- Array.make_matrix (Fmt.channels ())
                        (order * 2 + 1) 0. ;
                      yv <- Array.make_matrix (Fmt.channels ())
                        (order * 2 + 1) 0.
                | High_pass ->
                    (* Hi-pass filter *)
                    if debug then
                      Printf.printf "This is a hi-pass filter.\n" ;
                    for i = 0 to splane_numpoles - 1 do
                      splane_poles.(i) <- w1 /~ splane_poles.(i) ;
                    done;
                    for i = 0 to splane_numpoles - 1 do
                      (* also N zeros at (0,0) *)
                      splane_zeros <- Array.append splane_zeros [|cor 0.|] ;
                    done;
                    splane_numzeros <- splane_numpoles ;
                    xv <- Array.make_matrix (Fmt.channels ()) (order + 1) 0. ;
                    yv <- Array.make_matrix (Fmt.channels ()) (order + 1) 0.
                | Low_pass ->
                    (* Lo-pass filter *)
                    if debug then
                      Printf.printf "This is a lo-pass filter.\n" ;
                    for i = 0 to splane_numpoles - 1 do
                      splane_poles.(i) <- splane_poles.(i) *~ w1 ;
                    done;
                    splane_numzeros <- 0 ;
                    xv <- Array.make_matrix (Fmt.channels ()) (order + 1) 0. ;
                    yv <- Array.make_matrix (Fmt.channels ()) (order + 1) 0.
                | _ -> assert false ) ;
            (* Compute Z-plane zeros & poles using bilinear transform *)
            if debug then
              (
                Printf.printf "S-Plane zeros:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f %+.013f i." i a.re a.im) splane_zeros)))) ;
                Printf.printf "S-Plane poles:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f %+.013f i." i a.re a.im) splane_poles))))
              ) ;
            zplane_numpoles <- splane_numpoles ;
            zplane_numzeros <- splane_numzeros ;
            let blt a = Complex.div (Complex.add {re = 2. ; im = 0.} a)
                                    (Complex.sub {re = 2. ; im = 0.} a)
            in
              for i = 0 to zplane_numpoles - 1 do
                zplane_poles <- Array.append zplane_poles
                                             [|blt splane_poles.(i)|]
              done;
              for i = 0 to zplane_numzeros - 1 do
                zplane_zeros <- Array.append zplane_zeros
                                             [|blt splane_zeros.(i)|]
              done;
              while (zplane_numzeros < zplane_numpoles) do
                zplane_zeros <- Array.append zplane_zeros
                                             [|{re = -1.0 ; im = 0.}|] ;
                zplane_numzeros <- zplane_numzeros + 1
              done;
            if debug then
              (
                Printf.printf "Z-Plane zeros:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f %+.013f i." i a.re a.im) zplane_zeros)))) ;
                Printf.printf "Z-Plane poles:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f %+.013f i." i a.re a.im) zplane_poles))))
              )
        | Resonator ->
            (* Compute Z-plane zeros and poles (Resonator). Let's admit we're
            creating a bandpass filter, we'll transform later if needed. *)
            if debug then
              Printf.printf "This is a Resonator filter.\n" ;
            zplane_numpoles <- 2 ;
            zplane_numzeros <- 2 ;
            zplane_zeros <- [|cor 1. ; cor (-1.)|] ;
            (* where we want the peak to be *)
            let theta = 2. *. pi *. raw_alpha1 in
              if (qfactor == infinity) then
                (
                  if debug then
                    Printf.printf "Infinite Q factor!\n" ;
                  (* oscillator *)
                  let zp = {re = cos theta ; im = sin theta } in
                    zplane_poles <- [|zp ; Complex.conj zp|]
                )
              else
                (
                  (* must iterate to find exact pole positions *)
                  topcoeffs <- expand zplane_zeros zplane_numzeros ;
                  let r = exp (cor (-. theta /. (2. *. qfactor)))
                    and thm = ref theta
                    and th1 = ref 0.
                    and th2 = ref pi
                    and cvg = ref false
                  in
                    for i = 0 to 50 do
                      let zp = r *~ {re = cos !thm ; im = sin !thm} in
                        zplane_poles <- [|zp ; Complex.conj zp|] ;
                        botcoeffs <- expand zplane_poles zplane_numpoles ;
                        let g = evaluate topcoeffs zplane_numzeros
                                         botcoeffs zplane_numpoles
                                         {re = cos theta ; im = sin theta}
                        in
                          let phi = g.im /. g.re in (* approx to atan2 *)
                            if (phi > 0.) then
                              th2 := !thm
                            else
                              th1 := !thm ;
                            if (abs_float phi < 1e-10) then
                              cvg := true ;
                            thm := 0.5 *. (!th1 +. !th2)
                    done ;
                    (* if we failed to converge ... *)
                    assert !cvg
              ) ;
              xv <- Array.make_matrix (Fmt.channels ()) (zplane_numzeros) 0. ;
              yv <- Array.make_matrix (Fmt.channels ()) (zplane_numpoles) 0. ;
              (* Do we need to transform to Bandstop or Allpass? *)
              ( match filter_type with
                  | Band_stop ->
                      (* Band-stop filter *)
                      if debug then
                        Printf.printf "This is a band-stop filter.\n" ;
                      (* compute Z-plane pole & zero positions for bandstop
                      resonator (notch filter) *)
                      (* place zeros exactly *)
                      let zp = {re = cos theta ; im = sin theta } in
                        zplane_poles <- [|zp ; Complex.conj zp|]
                  | All_pass ->
                      (* All-pass filter *)
                      if debug then
                        Printf.printf "This is an all-pass filter.\n" ;
                      (* compute Z-plane pole & zero positions for allpass
                      resonator *)
                      zplane_zeros.(0) <- zplane_poles.(0) /~
                        sqrt (cor (Complex.norm zplane_poles.(0))) ;
                      zplane_zeros.(1) <- zplane_poles.(1) /~
                        sqrt (cor (Complex.norm zplane_poles.(1))) 
                  | Band_pass -> () ;
                  | _ -> assert false ) ) ;
      (* Now expand the polynomials *)
      Printf.printf "Expanding polynomials...\n" ;
      topcoeffs <- expand zplane_zeros zplane_numzeros ;
      botcoeffs <- expand zplane_poles zplane_numpoles ;
      if debug then
        (
          Printf.printf "Top coeffs:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f %+.013f i." i a.re a.im) topcoeffs)))) ;
          Printf.printf "Bottom coeffs:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f %+.013f i." i a.re a.im) botcoeffs))))
        ) ;
      (* Gain *)
      dc_gain <- evaluate topcoeffs zplane_numzeros botcoeffs
                          zplane_numpoles {re = 1. ; im = 0.} ;
      let theta =
        2. *. pi *. 0.5 *. (raw_alpha1 +. raw_alpha2) (* jwt for centre freq. *)
      in
        fc_gain <- evaluate topcoeffs zplane_numzeros botcoeffs zplane_numpoles
                     (Complex.exp {re = 0. ; im = theta}) ;
      hf_gain <- evaluate topcoeffs zplane_numzeros botcoeffs zplane_numpoles
                   {re = -1. ; im = 0.} ;
      gain <- ( match filter_type with
                | Band_stop -> Complex.norm (sqrt (dc_gain *~ hf_gain ))
                | Band_pass
                | All_pass -> Complex.norm fc_gain
                | High_pass -> Complex.norm hf_gain
                | Low_pass -> Complex.norm dc_gain ) ;
      if debug then
        Printf.printf "Gains:\nDC:     %+.013f.\nCentre: %+.013f.\nHF:     %+.013f.\nFinal:  %+.013f.\n" (Complex.norm dc_gain) (Complex.norm fc_gain) (Complex.norm hf_gain) gain ;
      (* X-coeffs *)
      for i = 0 to zplane_numzeros do
        xcoeffs <- Array.append xcoeffs
                     [|topcoeffs.(i).re /. botcoeffs.(zplane_numpoles).re|] ;
      done;
      if debug then
        Printf.printf "Xcoeffs:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f." i a) xcoeffs)))) ;
      (* Y-coeffs *)
      for i = 0 to zplane_numpoles do
        ycoeffs <- Array.append ycoeffs
              [|(0. -. (botcoeffs.(i).re) /. botcoeffs.(zplane_numpoles).re)|] ;
      done;
      if debug then
        (
          Printf.printf "Ycoeffs:\n%s\n" (String.concat "\n" (Array.to_list ((Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f." i a) ycoeffs)))) ;
          Printf.printf "================== END INIT FILTER %s ==================\n" self#id
        ) 
  
  (* Digital filter based on mkfilter/mkshape/gencode by A.J. Fisher *)

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  val mutable v_offs = 0

  method get_frame buf =
    let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.get_float_pcm buf in
      let v_len = Array.length xv.(0) in
      let coeffs_len = Array.length xcoeffs in
      let fold_left2 init v coeffs l =
        let l = min (Array.length v) (min l (Array.length coeffs)) in
        let result = ref init in
          for i = 0 to l - 1 do
            result := !result +. v.((i + v_offs) mod v_len) *. coeffs.(i)
          done;
          !result
      in
      for c = 0 to (Fmt.channels ()) - 1 do
        let xvc = xv.(c) in
        let yvc = yv.(c) in
        let bc = b.(c) in
          for i = offset to AFrame.position buf - 1 do
            v_offs <- (v_offs + 1) mod v_len;
            xvc.((coeffs_len - 1 + v_offs) mod v_len) <- bc.(i) /. gain ;
            let insert =
              fold_left2 0. xvc xcoeffs 110 (* TODO: why 110? *)
              +. fold_left2 0. yvc ycoeffs (coeffs_len - 1)
            in
              yvc.((coeffs_len - 1 + v_offs) mod v_len) <- insert ;
              bc.(i) <- insert
          done;
      done
end

let () =
  Lang.add_operator "filter.iir.butterworth.high"
    [
      "frequency", Lang.float_t, None, Some "Corner frequency" ;
      "order", Lang.int_t, Some (Lang.int 4), Some "Filter order" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"IIR filter"
    (fun p ->
       let f v = List.assoc v p in
       let freq, order, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_int (f "order"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new iir src Butterworth High_pass order freq 0. 0. debug):>source)
    ) ;

  Lang.add_operator "filter.iir.butterworth.low"
    [
      "frequency", Lang.float_t, None, Some "Corner frequency" ;
      "order", Lang.int_t, Some (Lang.int 4), Some "Filter order" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"IIR filter"
    (fun p ->
       let f v = List.assoc v p in
       let freq, order, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_int (f "order"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new iir src Butterworth Low_pass order freq 0. 0. debug):>source)
    ) ;

  Lang.add_operator "filter.iir.butterworth.bandpass"
    [
      "frequency1", Lang.float_t, None, Some "First corner frequency" ;
      "frequency2", Lang.float_t, None,
        Some "Second corner frequency" ;
      "order", Lang.int_t, Some (Lang.int 4), Some "Filter order" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"IIR filter"
    (fun p ->
       let f v = List.assoc v p in
       let freq1, freq2, order, debug, src =
         Lang.to_float (f "frequency1"),
         Lang.to_float (f "frequency2"),
         Lang.to_int (f "order"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new iir src Butterworth Band_pass order freq1 freq2 0. debug):>source)
    ) ;

  Lang.add_operator "filter.iir.butterworth.bandstop"
    [
      "frequency1", Lang.float_t, None, Some "First corner frequency" ;
      "frequency2", Lang.float_t, None,
        Some "Second corner frequency" ;
      "order", Lang.int_t, Some (Lang.int 4), Some "Filter order" ;
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"IIR filter"
    (fun p ->
       let f v = List.assoc v p in
       let freq1, freq2, order, debug, src =
         Lang.to_float (f "frequency1"),
         Lang.to_float (f "frequency2"),
         Lang.to_int (f "order"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new iir src Butterworth Band_stop order freq1 freq2 0. debug):>source)
    ) ;
    

  Lang.add_operator "filter.iir.resonator.bandpass"
    [
      "frequency", Lang.float_t, None, Some "Corner frequency" ;
      "q", Lang.float_t, Some (Lang.float 60.), Some "Quality factor" ;
(*      "order", Lang.int_t, Some (Lang.int 4), Some "Filter order" ;*)
      "debug", Lang.bool_t, Some (Lang.bool false), Some "Debug output" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"IIR filter"
    (fun p ->
       let f v = List.assoc v p in
       let freq, q, debug, src =
         Lang.to_float (f "frequency"),
         Lang.to_float (f "q"),
         Lang.to_bool (f "debug"),
         Lang.to_source (f "") in
         ((new iir src Resonator Band_pass 0 freq 0. q debug):>source)
    ) ;

(*  Lang.add_operator "filter.crossover"
    [
      "bands", Lang.list_t Lang.float_t,
        Some (Lang.list [ Lang.float 880. ; Lang.float 5000. ]),
        Some "Bands" ;
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"IIR filter"
    (fun p ->
       let f v = List.assoc v p in
       let freqs, src =
         Lang.to_float_list (f "bands"),
         Lang.to_source (f "")
       in
       let freq_to_filter f1 f2 =
         if f1 == 0. then
           Lang.Source ((new iir src Low_pass 4 f1 f2):>source)
         else if f2 == 0. then
           Lang.Source ((new iir src High_pass 4 f1 f2):>source)
         else
           Lang.Source ((new iir src Band_pass 4 f1 f2):>source)
       in
       Lang.List (List.map2 freq_to_filter
                 (0.::freqs) (List.append freqs [0.]))
    ) ;
*)
