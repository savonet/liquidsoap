(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
open Complex

class fir ~kind (source : source) freq beta numcoeffs =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  object (self)
    inherit operator ~name:"fir_filter" kind [source]

    (* Needed to compute RC *)
    val f1 = (1. -. beta) *. (freq /. float_of_int (Frame.audio_of_seconds 1.))

    val f2 = (1. +. beta) *. (freq /. float_of_int (Frame.audio_of_seconds 1.))

    val tau = 0.5 /. (freq /. float_of_int (Frame.audio_of_seconds 1.))

    (* Misc *)
    val mutable nzeros = numcoeffs - 1

    val mutable gain = 0.

    val mutable xv = Array.make_matrix channels numcoeffs 0.

    (* Coefficients *)
    val mutable xcoeffs = Array.make numcoeffs 0.

    val mutable circle = [||]

    val mutable temp = Array.make 2048 { re = 0.; im = 0. }

    initializer
    self#log#info
      "Init: alpha=%+.013f beta=%+.013f F1=%+.013f F2=%+.013f tau=%+.013f \
       zeros=%d."
      (freq /. float_of_int (Frame.audio_of_seconds 1.))
      beta f1 f2 tau nzeros;
    (* Init circle *)
    let circle =
      let rec mkcircle n =
        if n < 0 then [||]
        else (
          let theta = Utils.pi *. float_of_int n /. 1024. in
          Array.append
            (mkcircle (n - 1))
            [| { re = cos theta; im = sin theta } |] )
      in
      mkcircle 1024
    in
    (* Compute vec *)
    let vec = Array.make 2048 { re = 0.; im = 0. } in
    let c n =
      let f = float_of_int n /. 2048. in
      match (f <= f1, f <= f2) with
        | true, _ -> 1.
        | false, true ->
            0.5 *. (1. +. cos (Utils.pi *. tau /. beta *. (f -. f1)))
        | false, false -> 0.
    in
    for i = 0 to 1024 do
      vec.(i) <- { re = c i *. tau; im = 0. }
    done;
    for i = 1 to 1024 do
      vec.(2048 - i) <- vec.(i)
    done;
    (* FFT *)
    let ( +~ ), ( -~ ), ( *~ ) = (Complex.add, Complex.sub, Complex.mul) in
    let rec fft t d s n =
      if n > 1 then (
        let h = n / 2 in
        for i = 0 to h - 1 do
          !t.(s + i) <- !d.(s + (2 * i));
          (* even *)
          !t.(s + h + i) <- !d.(s + (2 * i) + 1) (* odd  *)
        done;
        fft d t s h;
        fft d t (s + h) h;
        let a = 2048 / n in
        for i = 0 to h - 1 do
          let wkt = circle.(i * a) *~ !t.(s + h + i) in
          !d.(s + i) <- !t.(s + i) +~ wkt;
          !d.(s + h + i) <- !t.(s + i) -~ wkt
        done )
    in
    fft (ref temp) (ref vec) 0 2048;
    (* inverse fft *)
    let h = (numcoeffs - 1) / 2 in
    xcoeffs <-
      Array.mapi (fun i _ -> vec.((2048 - h + i) mod 2048).re /. 2048.) xcoeffs;
    self#log#info "Xcoeffs: %s"
      (String.concat "\n"
         (Array.to_list
            (Array.mapi (fun i a -> Printf.sprintf "%d: %+.013f." i a) xcoeffs)));
    gain <- Array.fold_left ( +. ) 0. xcoeffs;
    self#log#info "Gain: %+.013f." gain;
    self#log#info "Init done."

    (* Digital filter based on mkfilter/mkshape/gencode by A.J. Fisher *)
    method stype = source#stype

    method remaining = source#remaining

    method is_ready = source#is_ready

    method seek = source#seek

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    method private get_frame buf =
      let offset = AFrame.position buf in
      source#get buf;
      let b = AFrame.content buf offset in
      let shift a =
        for i = 0 to Array.length a - 2 do
          a.(i) <- a.(i + 1)
        done
      in
      let fold_left2 f init a1 a2 =
        let l = min (Array.length a1) (Array.length a2) in
        let result = ref init in
        for i = 0 to l - 1 do
          result := f !result a1.(i) a2.(i)
        done;
        !result
      in
      let addtimes a b c = a +. (b *. c) in
      for c = 0 to 1 do
        for i = offset to AFrame.position buf - 1 do
          shift xv.(c);
          xv.(c).(nzeros) <- b.(c).{i} /. gain;
          b.(c).{i} <- fold_left2 addtimes 0. xcoeffs xv.(c)
        done
      done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "filter.fir"
    [
      ( "frequency",
        Lang.float_t,
        None,
        Some
          "Corner frequency in Hz (frequency at which the response is 0.5, \
           that is -6 dB)." );
      ("beta", Lang.float_t, None, Some "Beta should range between 0 and 1.");
      ("coeffs", Lang.int_t, Some (Lang.int 255), Some "Number of coefficients");
      ("", Lang.source_t k, None, None);
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Low-pass FIR filter."
    (fun p kind ->
      let f v = List.assoc v p in
      let freq, beta, num, src =
        ( Lang.to_float (f "frequency"),
          Lang.to_float (f "beta"),
          Lang.to_int (f "coeffs"),
          Lang.to_source (f "") )
      in
      new fir ~kind src freq beta num)
