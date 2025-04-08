(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

(* TODO:
   - lots of functions require offset and length whereas in most cases we
   want to apply the operations on the whole buffers -> labeled optional
   arguments?
   - do we want to pass samplerate as an argument or to store it in buffers? *)

open Mm_base

let list_filter_ctxt f l =
  let rec aux b = function
    | [] -> []
    | h :: t -> if f b h t then h :: aux (b @ [h]) t else aux (b @ [h]) t
  in
  aux [] l

let pi = 3.14159265358979323846
let lin_of_dB x = 10. ** (x /. 20.)
let dB_of_lin x = 20. *. log x /. log 10.

(** Fractional part of a float. *)
let fracf x = if x < 1. then x else if x < 2. then x -. 1. else fst (modf x)

let samples_of_seconds sr t = int_of_float (float sr *. t)
let seconds_of_samples sr n = float n /. float sr

module Note = struct
  (* A4 = 69 *)
  type t = int

  let a4 = 69
  let c5 = 72
  let c0 = 12
  let create name oct = name + (12 * (oct + 1))
  let freq n = 440. *. (2. ** ((float n -. 69.) /. 12.))

  let of_freq f =
    int_of_float (0.5 +. ((12. *. log (f /. 440.) /. log 2.) +. 69.))

  let name n = n mod 12
  let octave n = (n / 12) - 1
  let modulo n = (name n, octave n)

  let to_string n =
    let n, o = modulo n in
    (match n with
      | 0 -> "C"
      | 1 -> "C#"
      | 2 -> "D"
      | 3 -> "D#"
      | 4 -> "E"
      | 5 -> "F"
      | 6 -> "F#"
      | 7 -> "G"
      | 8 -> "G#"
      | 9 -> "A"
      | 10 -> "A#"
      | 11 -> "B"
      | _ -> assert false)
    ^ " " ^ string_of_int o

  (* TODO: sharps and flats *)
  let of_string s =
    assert (String.length s >= 2);
    let note = String.sub s 0 (String.length s - 1) in
    let oct = int_of_char s.[String.length s - 1] - int_of_char '0' in
    let off =
      match note with
        | "a" | "A" -> 0
        | "b" | "B" -> 2
        | "c" | "C" -> 3
        | "d" | "D" -> 5
        | "e" | "E" -> 7
        | "f" | "F" -> 8
        | "g" | "G" -> 10
        | _ -> raise Not_found
    in
    64 + (12 * (oct - 4)) + off
end

module Sample = struct
  type t = float

  let clip x =
    let x = max (-1.) x in
    let x = min 1. x in
    x

  let iir a b =
    let na = Array.length a in
    let nb = Array.length b in
    assert (a.(0) = 1.);
    let x = Array.make nb 0. in
    let y = Array.make na 0. in
    let ka = ref 0 in
    let kb = ref 0 in
    fun x0 ->
      let y0 = ref 0. in
      x.(!kb) <- x0;
      for i = 0 to nb - 1 do
        y0 := !y0 +. (b.(i) *. x.((!kb + i) mod nb))
      done;
      for i = 1 to na - 1 do
        y0 := !y0 -. (a.(i) *. y.((!ka + i) mod na))
      done;
      if na > 0 then y.(!ka) <- !y0;
      let decr n k =
        decr k;
        if !k < 0 then k := !k + n
      in
      decr na ka;
      decr nb kb;
      !y0

  let fir b = iir [||] b
end

module Mono = struct
  type t = float array
  type buffer = t

  let create = Array.create_float
  let length = Array.length
  let buffer_length = length
  let clear data ofs len = Array.fill data ofs len 0.
  let make n (x : float) = Array.make n x
  let sub = Array.sub
  let blit = Array.blit

  let copy src ofs len =
    let dst = create len in
    blit src ofs dst 0 len;
    dst

  external copy_from_ba :
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    float array ->
    int ->
    int ->
    unit = "caml_mm_audio_copy_from_ba"

  external copy_to_ba :
    float array ->
    int ->
    int ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit = "caml_mm_audio_copy_to_ba"

  let of_ba buf =
    let len = Bigarray.Array1.dim buf in
    let dst = Array.create_float len in
    copy_from_ba buf dst 0 len;
    dst

  let to_ba buf ofs len =
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout len in
    copy_to_ba buf ofs len ba;
    ba

  external copy_from_int16_ba :
    (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    float array ->
    int ->
    int ->
    unit = "caml_mm_audio_copy_from_int16_ba"

  external copy_to_int16_ba :
    float array ->
    int ->
    int ->
    (int, Bigarray.int16_signed_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit = "caml_mm_audio_copy_to_int16_ba"

  let of_int16_ba buf =
    let len = Bigarray.Array1.dim buf in
    let dst = Array.create_float len in
    copy_from_int16_ba buf dst 0 len;
    dst

  let to_int16_ba buf ofs len =
    let ba =
      Bigarray.Array1.create Bigarray.int16_signed Bigarray.c_layout len
    in
    copy_to_int16_ba buf ofs len ba;
    ba

  let append b1 ofs1 len1 b2 ofs2 len2 =
    assert (length b1 - ofs1 >= len1);
    assert (length b2 - ofs2 >= len2);
    let data = Array.create_float (len1 + len2) in
    Array.blit b1 ofs1 data 0 len1;
    Array.blit b2 ofs2 data len1 len2;
    data

  let add b1 ofs1 b2 ofs2 len =
    assert (length b1 - ofs1 >= len);
    assert (length b2 - ofs2 >= len);
    for i = 0 to len - 1 do
      Array.unsafe_set b1 (ofs1 + i)
        (Array.unsafe_get b1 (ofs1 + i) +. Array.unsafe_get b2 (ofs2 + i))
    done

  let add_coeff b1 ofs1 k b2 ofs2 len =
    assert (length b1 - ofs1 >= len);
    assert (length b2 - ofs2 >= len);
    for i = 0 to len - 1 do
      Array.unsafe_set b1 (ofs1 + i)
        (Array.unsafe_get b1 (ofs1 + i) +. (k *. Array.unsafe_get b2 (ofs2 + i)))
    done

  let add_coeff b1 ofs1 k b2 ofs2 len =
    if k = 0. then ()
    else if k = 1. then add b1 ofs1 b2 ofs2 len
    else add_coeff b1 ofs1 k b2 ofs2 len

  let mult b1 ofs1 b2 ofs2 len =
    assert (length b1 - ofs1 >= len);
    assert (length b2 - ofs2 >= len);
    for i = 0 to len - 1 do
      Array.unsafe_set b1 (ofs1 + i)
        (Array.unsafe_get b1 (ofs1 + i) *. Array.unsafe_get b2 (ofs2 + i))
    done

  let amplify c b ofs len =
    assert (length b - ofs >= len);
    for i = 0 to len - 1 do
      Array.unsafe_set b (ofs + i) (Array.unsafe_get b (ofs + i) *. c)
    done

  let clip b ofs len =
    assert (length b - ofs >= len);
    for i = 0 to len - 1 do
      let s = Array.unsafe_get b (ofs + i) in
      Array.unsafe_set b (ofs + i)
        (if Float.is_nan s then 0.
         else if s < -1. then -1.
         else if 1. < s then 1.
         else s)
    done

  let squares b ofs len =
    assert (length b - ofs >= len);
    let ret = ref 0. in
    for i = 0 to len - 1 do
      let s = Array.unsafe_get b (ofs + i) in
      ret := !ret +. (s *. s)
    done;
    !ret

  let noise b ofs len =
    assert (length b - ofs >= len);
    for i = 0 to len - 1 do
      Array.unsafe_set b (ofs + i) (Random.float 2. -. 1.)
    done

  let resample ?(mode = `Linear) ratio inbuf ofs len =
    assert (length inbuf - ofs >= len);
    if ratio = 1. then copy inbuf ofs len
    else if mode = `Nearest then (
      let outlen = int_of_float ((float len *. ratio) +. 0.5) in
      let outbuf = create outlen in
      for i = 0 to outlen - 1 do
        let pos = min (int_of_float ((float i /. ratio) +. 0.5)) (len - 1) in
        Array.unsafe_set outbuf i (Array.unsafe_get inbuf (ofs + pos))
      done;
      outbuf)
    else (
      let outlen = int_of_float (float len *. ratio) in
      let outbuf = create outlen in
      for i = 0 to outlen - 1 do
        let ir = float i /. ratio in
        let pos = min (int_of_float ir) (len - 1) in
        if pos = len - 1 then
          Array.unsafe_set outbuf i (Array.unsafe_get inbuf (ofs + pos))
        else (
          let a = ir -. float pos in
          Array.unsafe_set outbuf i
            ((Array.unsafe_get inbuf (ofs + pos) *. (1. -. a))
            +. (Array.unsafe_get inbuf (ofs + pos + 1) *. a)))
      done;
      outbuf)

  module B = struct
    type t = buffer

    let create = create
    let blit = blit
  end

  module Ringbuffer_ext = Ringbuffer.Make_ext (B)
  module Ringbuffer = Ringbuffer.Make (B)

  (* TODO: refined allocation/deallocation policies *)
  module Buffer_ext = struct
    type t = { mutable buffer : buffer }

    let prepare buf len =
      if length buf.buffer >= len then sub buf.buffer 0 len
      else (
        (* TODO: optionally blit the old buffer onto the new one. *)
        (* let oldbuf = buf.buffer in *)
        let newbuf = create len in
        buf.buffer <- newbuf;
        newbuf)

    let create len = { buffer = create len }
    let length buf = length buf.buffer
  end

  module Analyze = struct
    let rms buf ofs len =
      let r = ref 0. in
      for i = 0 to len - 1 do
        let x = buf.(i + ofs) in
        r := !r +. (x *. x)
      done;
      sqrt (!r /. float len)

    module FFT = struct
      type t = {
        b : int;
        (* number of bits *)
        n : int;
        (* number of samples *)
        circle : Complex.t array;
        temp : Complex.t array;
      }

      let init b =
        let n = 1 lsl b in
        let h = n / 2 in
        let fh = float h in
        let circle = Array.make h Complex.zero in
        for i = 0 to h - 1 do
          let theta = pi *. float_of_int i /. fh in
          circle.(i) <- { Complex.re = cos theta; Complex.im = sin theta }
        done;
        { b; n; circle; temp = Array.make n Complex.zero }

      let length f = f.n

      let complex_create buf ofs len =
        Array.init len (fun i ->
            { Complex.re = buf.(ofs + i); Complex.im = 0. })

      let ccoef k c =
        { Complex.re = k *. c.Complex.re; Complex.im = k *. c.Complex.im }

      let fft f d =
        (* TODO: greater should be ok too? *)
        assert (Array.length d = f.n);
        let ( +~ ) = Complex.add in
        let ( -~ ) = Complex.sub in
        let ( *~ ) = Complex.mul in
        let rec fft t (* temporary buffer *) d (* data *) s
            (* stride in the data array *) n (* number of samples *) =
          if n > 1 then (
            let h = n / 2 in
            for i = 0 to h - 1 do
              t.(s + i) <- d.(s + (2 * i));
              (* even *)
              t.(s + h + i) <- d.(s + (2 * i) + 1)
              (* odd  *)
            done;
            fft d t s h;
            fft d t (s + h) h;
            let a = f.n / n in
            for i = 0 to h - 1 do
              let wkt = f.circle.(i * a) *~ t.(s + h + i) in
              d.(s + i) <- t.(s + i) +~ wkt;
              d.(s + h + i) <- t.(s + i) -~ wkt
            done)
        in
        fft f.temp d 0 f.n

      (* See http://en.wikipedia.org/wiki/Window_function *)
      module Window = struct
        let iter f d =
          let len = Array.length d in
          let n = float len in
          for i = 0 to len - 1 do
            let k = f (float i) n in
            d.(i) <- ccoef k d.(i)
          done

        let hann d = iter (fun i n -> 0.5 *. (1. -. cos (2. *. pi *. i /. n))) d

        let hamming d =
          iter (fun i n -> 0.54 *. (0.46 *. cos (2. *. pi *. i /. n))) d

        let cosine d = iter (fun i n -> sin (pi *. i /. n)) d

        let lanczos d =
          let sinc x =
            let px = pi *. x in
            sin px /. px
          in
          iter (fun i n -> sinc (2. *. i /. n)) d

        let triangular d =
          iter
            (fun i n ->
              if i <= n /. 2. then 2. *. i /. n else ((n /. 2.) -. i) *. 2. /. n)
            d

        let bartlett_hann d =
          let a0 = 0.62 in
          let a1 = 0.48 in
          let a2 = 0.38 in
          iter
            (fun i n ->
              a0
              -. (a1 *. abs_float ((i /. n) -. 0.5))
              -. (a2 *. cos (2. *. pi *. i /. n)))
            d

        let blackman ?(alpha = 0.16) d =
          let a = alpha in
          let a0 = (1. -. a) /. 2. in
          let a1 = 1. /. 2. in
          let a2 = a /. 2. in
          iter
            (fun i n ->
              a0
              -. (a1 *. cos (2. *. pi *. i /. n))
              +. (a2 *. cos (4. *. pi *. i /. n)))
            d

        (* TODO: use circle to compute cosines *)
        let low_res a0 a1 a2 a3 d =
          iter
            (fun i n ->
              a0
              -. (a1 *. cos (2. *. pi *. i /. n))
              +. (a2 *. cos (4. *. pi *. i /. n))
              -. (a3 *. cos (6. *. pi *. i /. n)))
            d

        let nuttall d = low_res 0.355768 0.487396 0.144232 0.012604 d
        let blackman_harris d = low_res 0.35875 0.48829 0.14128 0.01168 d

        let blackman_nuttall d =
          low_res 0.3635819 0.4891775 0.1365995 0.0106411 d
      end

      let band_freq sr f k = float k *. float sr /. float f.n

      let notes sr f ?(note_min = Note.c0) ?(note_max = 128)
          ?(volume_min = 0.01) ?(filter_harmonics = true) buf =
        let len = buffer_length buf in
        assert (len = length f);
        let bdur = float len /. float sr in
        let fdf = float (length f) in
        let c = complex_create buf 0 len in
        fft f c;
        let ans = ref [] in
        let kstart = max 0 (int_of_float (Note.freq note_min *. bdur)) in
        let kend = min (len / 2) (int_of_float (Note.freq note_max *. bdur)) in
        for k = kstart + 1 to kend - 2 do
          (* Quadratic interpolation. *)
          let v' = Complex.norm c.(k - 1) in
          let v = Complex.norm c.(k) in
          let v'' = Complex.norm c.(k - 1) in
          (* Do we have a maximum here? *)
          if v' +. v'' < 2. *. v then (
            let p = (v'' -. v') /. ((2. *. v') -. (2. *. v) +. v'') in
            let v = v -. ((v' -. v'') *. p /. 4.) in
            let v = v /. fdf in
            let p = p +. float k in
            if v >= volume_min then ans := (p, v) :: !ans)
        done;
        let ans = List.map (fun (k, v) -> (Note.of_freq (k /. bdur), v)) !ans in
        (* TODO: improve this filtering... *)
        let ans =
          if filter_harmonics then
            list_filter_ctxt
              (fun b (n, _) t ->
                let o = Note.octave n in
                let n = Note.name n in
                List.for_all
                  (fun (n', _) -> Note.name n' <> n || Note.octave n' >= o)
                  (b @ t))
              ans
          else ans
        in
        ans

      let loudest_note l =
        match l with
          | [] -> None
          | h :: t ->
              Some
                (List.fold_left
                   (fun (nmax, vmax) (n, v) ->
                     if v > vmax then (n, v) else (nmax, vmax))
                   h t)
    end
  end

  module Effect = struct
    let compand_mu_law mu buf ofs len =
      for i = 0 to len - 1 do
        let bufi = buf.(i + ofs) in
        let sign = if bufi < 0. then -1. else 1. in
        buf.(i + ofs) <-
          sign *. log (1. +. (mu *. abs_float bufi)) /. log (1. +. mu)
      done

    class type t = object
      method process : buffer -> int -> int -> unit
    end

    class amplify k : t =
      object
        method process = amplify k
      end

    class clip c : t =
      object
        method process buf ofs len =
          for i = 0 to len - 1 do
            Array.unsafe_set buf (i + ofs)
              (max (-.c) (min c (Array.unsafe_get buf (i + ofs))))
          done
      end

    (* Digital filter based on "Cookbook formulae for audio EQ biquad filter
       coefficients" by Robert Bristow-Johnson <rbj@audioimagination.com>.  URL:
       http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt *)
    class biquad_filter samplerate
      (kind :
        [ `Low_pass
        | `High_pass
        | `Band_pass
        | `Notch
        | `All_pass
        | `Peaking
        | `Low_shelf
        | `High_shelf ]) ?(gain = 0.) freq q =
      let samplerate = float samplerate in
      object (self)
        val mutable p0 = 0.
        val mutable p1 = 0.
        val mutable p2 = 0.
        val mutable q1 = 0.
        val mutable q2 = 0.

        method private init =
          let w0 = 2. *. pi *. freq /. samplerate in
          let cos_w0 = cos w0 in
          let sin_w0 = sin w0 in
          let alpha = sin w0 /. (2. *. q) in
          let a = if gain = 0. then 1. else 10. ** (gain /. 40.) in
          let b0, b1, b2, a0, a1, a2 =
            match kind with
              | `Low_pass ->
                  let b1 = 1. -. cos_w0 in
                  let b0 = b1 /. 2. in
                  (b0, b1, b0, 1. +. alpha, -2. *. cos_w0, 1. -. alpha)
              | `High_pass ->
                  let b1 = 1. +. cos_w0 in
                  let b0 = b1 /. 2. in
                  let b1 = -.b1 in
                  (b0, b1, b0, 1. +. alpha, -2. *. cos_w0, 1. -. alpha)
              | `Band_pass ->
                  let b0 = sin_w0 /. 2. in
                  (b0, 0., -.b0, 1. +. alpha, -2. *. cos_w0, 1. -. alpha)
              | `Notch ->
                  let b1 = -2. *. cos_w0 in
                  (1., b1, 1., 1. +. alpha, b1, 1. -. alpha)
              | `All_pass ->
                  let b0 = 1. -. alpha in
                  let b1 = -2. *. cos_w0 in
                  let b2 = 1. +. alpha in
                  (b0, b1, b2, b2, b1, b0)
              | `Peaking ->
                  let ama = alpha *. a in
                  let ada = alpha /. a in
                  let b1 = -2. *. cos_w0 in
                  (1. +. ama, b1, 1. -. ama, 1. +. ada, b1, 1. -. ada)
              | `Low_shelf ->
                  let s = 2. *. sqrt a *. alpha in
                  ( a *. (a +. 1. -. ((a -. 1.) *. cos_w0) +. s),
                    2. *. a *. (a -. 1. -. ((a +. 1.) *. cos_w0)),
                    a *. (a +. 1. -. ((a -. 1.) *. cos_w0) -. s),
                    a +. 1. +. ((a -. 1.) *. cos_w0) +. s,
                    (-2. *. (a -. 1.)) +. ((a +. 1.) *. cos_w0),
                    a +. 1. +. ((a -. 1.) *. cos_w0) -. s )
              | `High_shelf ->
                  let s = 2. *. sqrt a *. alpha in
                  ( a *. (a +. 1. +. ((a -. 1.) *. cos_w0) +. s),
                    -2. *. a *. (a -. 1. +. ((a +. 1.) *. cos_w0)),
                    a *. (a +. 1. +. ((a -. 1.) *. cos_w0) -. s),
                    a +. 1. -. ((a -. 1.) *. cos_w0) +. s,
                    (2. *. (a -. 1.)) -. ((a +. 1.) *. cos_w0),
                    a +. 1. -. ((a -. 1.) *. cos_w0) -. s )
          in
          p0 <- b0 /. a0;
          p1 <- b1 /. a0;
          p2 <- b2 /. a0;
          q1 <- a1 /. a0;
          q2 <- a2 /. a0

        initializer self#init
        val mutable x1 = 0.
        val mutable x2 = 0.
        val mutable y0 = 0.
        val mutable y1 = 0.
        val mutable y2 = 0.

        method process (buf : buffer) ofs len =
          for i = 0 to len - 1 do
            let x0 = buf.(i + ofs) in
            let y0 =
              (p0 *. x0) +. (p1 *. x1) +. (p2 *. x2) -. (q1 *. y1) -. (q2 *. y2)
            in
            buf.(i + ofs) <- y0;
            x2 <- x1;
            x1 <- x0;
            y2 <- y1;
            y1 <- y0
          done
      end

    module ADSR = struct
      type t = int * int * float * int

      (** Convert adsr in seconds to samples. *)
      let make sr (a, d, s, r) =
        ( samples_of_seconds sr a,
          samples_of_seconds sr d,
          s,
          samples_of_seconds sr r )

      (** State in the ADSR envelope (A/D/S/R/dead + position in the state). *)
      type state = int * int

      let init () = (0, 0)
      let release (_, p) = (3, p)
      let dead (s, _) = s = 4

      let rec process adsr st (buf : buffer) ofs len =
        let a, (d : int), s, (r : int) = adsr in
        let state, state_pos = st in
        match state with
          | 0 ->
              let fa = float a in
              for i = 0 to min len (a - state_pos) - 1 do
                buf.(i + ofs) <- float (state_pos + i) /. fa *. buf.(i + ofs)
              done;
              if len < a - state_pos then (0, state_pos + len)
              else
                process adsr (1, 0) buf
                  (ofs + a - state_pos)
                  (len - (a - state_pos))
          | 1 ->
              let fd = float d in
              for i = 0 to min len (d - state_pos) - 1 do
                buf.(i + ofs) <-
                  (1. -. (float (state_pos + i) /. fd *. (1. -. s)))
                  *. buf.(i + ofs)
              done;
              if len < d - state_pos then (1, state_pos + len)
              else if
                (* Negative sustain means release immediately. *)
                s >= 0.
              then
                process adsr (2, 0) buf
                  (ofs + d - state_pos)
                  (len - (d - state_pos))
              else
                process adsr (3, 0) buf
                  (ofs + d - state_pos)
                  (len - (d - state_pos))
          | 2 ->
              amplify s buf ofs len;
              st
          | 3 ->
              let fr = float r in
              for i = 0 to min len (r - state_pos) - 1 do
                buf.(i + ofs) <-
                  s *. (1. -. (float (state_pos + i) /. fr)) *. buf.(i + ofs)
              done;
              if len < r - state_pos then (3, state_pos + len)
              else
                process adsr (4, 0) buf
                  (ofs + r - state_pos)
                  (len - (r - state_pos))
          | 4 ->
              clear buf ofs len;
              st
          | _ -> assert false
    end
  end

  module Generator = struct
    let white_noise buf = noise buf

    class type t = object
      method set_volume : float -> unit
      method set_frequency : float -> unit
      method fill : buffer -> int -> int -> unit
      method fill_add : buffer -> int -> int -> unit
      method release : unit
      method dead : bool
    end

    class virtual base sample_rate ?(volume = 1.) freq =
      object (self)
        val mutable vol = volume
        val mutable freq : float = freq
        val mutable dead = false
        method dead = dead

        method release =
          vol <- 0.;
          dead <- true

        method private sample_rate : int = sample_rate
        method private volume : float = vol
        method set_volume v = vol <- v
        method set_frequency f = freq <- f
        method virtual fill : buffer -> int -> int -> unit

        (* TODO: might be optimized by various synths *)
        method fill_add (buf : buffer) ofs len =
          let tmp = create len in
          self#fill tmp 0 len;
          add buf ofs tmp 0 len
      end

    class white_noise ?volume sr =
      object (self)
        inherit base sr ?volume 0.

        method fill buf ofs len =
          let volume = self#volume in
          for i = 0 to len - 1 do
            buf.(i + ofs) <- volume *. (Random.float 2. -. 1.)
          done
      end

    class sine sr ?volume ?(phase = 0.) freq =
      object (self)
        inherit base sr ?volume freq
        val mutable phase = phase

        method fill buf ofs len =
          let sr = float self#sample_rate in
          let omega = 2. *. pi *. freq /. sr in
          let volume = self#volume in
          for i = 0 to len - 1 do
            buf.(i + ofs) <- volume *. sin ((float i *. omega) +. phase)
          done;
          phase <- mod_float (phase +. (float len *. omega)) (2. *. pi)
      end

    class square sr ?volume ?(phase = 0.) freq =
      object (self)
        inherit base sr ?volume freq
        val mutable phase = phase

        method fill buf ofs len =
          let sr = float self#sample_rate in
          let volume = self#volume in
          let omega = freq /. sr in
          for i = 0 to len - 1 do
            let t = fracf ((float i *. omega) +. phase) in
            buf.(i + ofs) <- (if t < 0.5 then volume else -.volume)
          done;
          phase <- mod_float (phase +. (float len *. omega)) 1.
      end

    class saw sr ?volume ?(phase = 0.) freq =
      object (self)
        inherit base sr ?volume freq
        val mutable phase = phase

        method fill buf ofs len =
          let volume = self#volume in
          let sr = float self#sample_rate in
          let omega = freq /. sr in
          for i = 0 to len - 1 do
            let t = fracf ((float i *. omega) +. phase) in
            buf.(i + ofs) <- volume *. ((2. *. t) -. 1.)
          done;
          phase <- mod_float (phase +. (float len *. omega)) 1.
      end

    class triangle sr ?volume ?(phase = 0.) freq =
      object (self)
        inherit base sr ?volume freq
        val mutable phase = phase

        method fill buf ofs len =
          let sr = float self#sample_rate in
          let volume = self#volume in
          let omega = freq /. sr in
          for i = 0 to len - 1 do
            let t = fracf ((float i *. omega) +. phase +. 0.25) in
            buf.(i + ofs) <-
              (volume
              *. if t < 0.5 then (4. *. t) -. 1. else (4. *. (1. -. t)) -. 1.)
          done;
          phase <- mod_float (phase +. (float len *. omega)) 1.
      end

    class chain (g : t) (e : Effect.t) : t =
      object
        method fill buf ofs len =
          g#fill buf ofs len;
          e#process buf ofs len

        val tmpbuf = Buffer_ext.create 0

        method fill_add (buf : buffer) ofs len =
          let tmpbuf = Buffer_ext.prepare tmpbuf len in
          g#fill tmpbuf 0 len;
          add buf ofs tmpbuf 0 len

        method set_volume = g#set_volume
        method set_frequency = g#set_frequency
        method release = g#release
        method dead = g#dead
      end

    class combine f (g1 : t) (g2 : t) : t =
      object
        val tmpbuf = Buffer_ext.create 0
        val tmpbuf2 = Buffer_ext.create 0

        method fill buf ofs len =
          g1#fill buf ofs len;
          let tmpbuf = Buffer_ext.prepare tmpbuf len in
          g2#fill tmpbuf 0 len;
          f buf ofs tmpbuf 0 len

        method fill_add buf ofs len =
          let tmpbuf = Buffer_ext.prepare tmpbuf len in
          g1#fill tmpbuf 0 len;
          let tmpbuf2 = Buffer_ext.prepare tmpbuf2 len in
          g2#fill tmpbuf2 0 len;
          f tmpbuf 0 tmpbuf2 0 len;
          add buf ofs tmpbuf 0 len

        method set_volume v =
          g1#set_volume v;
          g2#set_volume v

        method set_frequency v =
          g1#set_frequency v;
          g2#set_frequency v

        method release =
          g1#release;
          g2#release

        method dead = g1#dead && g2#dead
      end

    class add g1 g2 =
      object
        inherit combine add g1 g2
      end

    class mult g1 g2 =
      object
        inherit combine mult g1 g2
      end

    class adsr (adsr : Effect.ADSR.t) (g : t) =
      object (self)
        val mutable adsr_st = Effect.ADSR.init ()
        val tmpbuf = Buffer_ext.create 0
        method set_volume = g#set_volume
        method set_frequency = g#set_frequency

        method fill buf ofs len =
          g#fill buf ofs len;
          adsr_st <- Effect.ADSR.process adsr adsr_st buf ofs len

        method fill_add buf ofs len =
          let tmpbuf = Buffer_ext.prepare tmpbuf len in
          self#fill tmpbuf 0 len;
          blit tmpbuf 0 buf ofs len

        method release =
          adsr_st <- Effect.ADSR.release adsr_st;
          g#release

        method dead = Effect.ADSR.dead adsr_st || g#dead
      end
  end
end

(** An audio buffer. *)
type t = Mono.buffer array

type buffer = t

(** Iterate a function on each channel of the buffer. *)
let iter f data offset length = Array.iter (fun b -> f b offset length) data

let create chans n = Array.init chans (fun _ -> Mono.create n)
let make chans n x = Array.init chans (fun _ -> Mono.make n x)
let channels data = Array.length data
let length = function [||] -> 0 | a -> Array.length a.(0)
let create_same buf = create (channels buf) (length buf)

(* TODO: in C *)
let interleave data length offset =
  let chans = Array.length data in
  let ibuf = Mono.create (chans * length) in
  for c = 0 to chans - 1 do
    let bufc = data.(c) in
    for i = 0 to length - 1 do
      ibuf.((chans * i) + c) <- bufc.(offset + i)
    done
  done;
  ibuf

(* TODO: in C *)
let deinterleave chans ibuf ofs len =
  let len = len / chans in
  let buf = create chans len in
  for c = 0 to chans - 1 do
    let bufc = buf.(c) in
    for i = 0 to len - 1 do
      bufc.(i) <- ibuf.((chans * i) + c + ofs)
    done
  done;
  buf

let append b1 ofs1 len1 b2 ofs2 len2 =
  Array.mapi (fun i b -> Mono.append b ofs1 len2 b2.(i) ofs2 len1) b1

let clear = iter Mono.clear
let clip = iter Mono.clip
let noise = iter Mono.noise

let copy b ofs len =
  Array.init (Array.length b) (fun i -> Mono.copy b.(i) ofs len)

let blit b1 ofs1 b2 ofs2 len =
  Array.iteri (fun i b -> Mono.blit b ofs1 b2.(i) ofs2 len) b1

let sub b ofs len = Array.map (fun b -> Array.sub b ofs len) b

let squares data offset length =
  Array.fold_left
    (fun squares buf -> squares +. Mono.squares buf offset length)
    0. data

let to_mono b ofs len =
  let channels = channels b in
  if channels = 1 then Array.sub b.(0) ofs len
  else (
    let chans = float channels in
    let ans = Mono.create len in
    Mono.clear ans 0 len;
    for i = 0 to len - 1 do
      for c = 0 to channels - 1 do
        ans.(i) <- ans.(i) +. b.(c).(i + ofs)
      done;
      ans.(i) <- ans.(i) /. chans
    done;
    ans)

let of_mono b = [| b |]

let resample ?mode ratio data offset length =
  Array.map (fun buf -> Mono.resample ?mode ratio buf offset length) data

let copy_from_ba ba buf ofs len =
  Array.iteri (fun i b -> Mono.copy_from_ba ba.(i) b ofs len) buf

let copy_to_ba buf ofs len ba =
  Array.iteri (fun i b -> Mono.copy_to_ba buf.(i) ofs len b) ba

let of_ba = Array.map Mono.of_ba
let to_ba buf ofs len = Array.map (fun b -> Mono.to_ba b ofs len) buf

let copy_from_int16_ba ba buf ofs len =
  Array.iteri (fun i b -> Mono.copy_from_int16_ba ba.(i) b ofs len) buf

let copy_to_int16_ba buf ofs len ba =
  Array.iteri (fun i b -> Mono.copy_to_int16_ba buf.(i) ofs len b) ba

let of_int16_ba = Array.map Mono.of_int16_ba

let to_int16_ba buf ofs len =
  Array.map (fun b -> Mono.to_int16_ba b ofs len) buf

module U8 = struct
  let size channels samples = channels * samples

  external of_audio : buffer -> int -> Bytes.t -> int -> int -> unit
    = "caml_mm_audio_to_u8"

  external to_audio : string -> int -> buffer -> int -> int -> unit
    = "caml_mm_audio_of_u8"
end

external to_s16 : bool -> buffer -> int -> Bytes.t -> int -> int -> unit
  = "caml_mm_audio_to_s16_byte" "caml_mm_audio_to_s16"

external convert_s16 : bool -> string -> int -> buffer -> int -> int -> unit
  = "caml_mm_audio_convert_s16_byte" "caml_mm_audio_convert_s16"

module S16LE = struct
  let size channels samples = channels * samples * 2
  let length channels len = len / (2 * channels)
  let of_audio = to_s16 true

  let make buf ofs len =
    let slen = size (channels buf) len in
    let sbuf = Bytes.create slen in
    of_audio buf ofs sbuf 0 len;
    Bytes.unsafe_to_string sbuf

  let to_audio = convert_s16 true
end

module S16BE = struct
  let size channels samples = channels * samples * 2
  let length channels len = len / (2 * channels)
  let of_audio = to_s16 false

  let make buf ofs len =
    let slen = size (channels buf) len in
    let sbuf = Bytes.create slen in
    of_audio buf ofs sbuf 0 len;
    Bytes.unsafe_to_string sbuf

  let to_audio = convert_s16 false
end

module S24LE = struct
  let size channels samples = channels * samples * 3

  external of_audio : buffer -> int -> Bytes.t -> int -> int -> unit
    = "caml_mm_audio_to_s24le"

  external to_audio : string -> int -> buffer -> int -> int -> unit
    = "caml_mm_audio_convert_s24le"
end

module S32LE = struct
  let size channels samples = channels * samples * 4

  external of_audio : buffer -> int -> Bytes.t -> int -> int -> unit
    = "caml_mm_audio_to_s32le"

  external to_audio : string -> int -> buffer -> int -> int -> unit
    = "caml_mm_audio_convert_s32le"
end

module FLTP = struct
  external of_audio :
    src:buffer ->
    src_offset:int ->
    dst:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    dst_offset:int ->
    len:int ->
    stride:int ->
    unit = "caml_mm_audio_to_fltp_bytes" "caml_mm_audio_to_fltp"

  external to_audio :
    src:(float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    src_offset:int ->
    dst:buffer ->
    dst_offset:int ->
    len:int ->
    stride:int ->
    unit = "caml_mm_audio_convert_fltp_bytes" "caml_mm_audio_convert_fltp"
end

let add b1 ofs1 b2 ofs2 len =
  Array.iteri (fun i b -> Mono.add b ofs1 b2.(i) ofs2 len) b1

let add_coeff b1 ofs1 k b2 ofs2 len =
  Array.iteri (fun i b -> Mono.add_coeff b ofs1 k b2.(i) ofs2 len) b1

let amplify k data offset length =
  if k <> 1. then
    Array.iter (fun data -> Mono.amplify k data offset length) data

(* x between -1 and 1 *)
let pan x buf offset length =
  if x > 0. then (
    let x = 1. -. x in
    Mono.amplify x buf.(0) offset length)
  else if x < 0. then (
    let x = 1. +. x in
    Mono.amplify x buf.(1) offset length)

(* TODO: we cannot share this with mono, right? *)
module Buffer_ext = struct
  type t = { mutable buffer : buffer }

  let chans = channels

  let prepare buf ?channels len =
    match channels with
      | Some channels when chans buf.buffer <> channels ->
          let newbuf = create channels len in
          buf.buffer <- newbuf;
          newbuf
      | _ ->
          if length buf.buffer >= len then sub buf.buffer 0 len
          else (
            (* TODO: optionally blit the old buffer onto the new one. *)
            let oldbuf = buf.buffer in
            let newbuf = create (chans oldbuf) len in
            buf.buffer <- newbuf;
            newbuf)

  let create chans len = { buffer = create chans len }
end

(* TODO: share code with ringbuffer module! *)
module Ringbuffer = struct
  type t = {
    size : int;
    buffer : buffer;
    mutable rpos : int;  (** current read position *)
    mutable wpos : int;  (** current write position *)
  }

  let create chans size =
    {
      (* size + 1 so we can store full buffers, while keeping
         rpos and wpos different for implementation matters *)
      size = size + 1;
      buffer = create chans (size + 1);
      rpos = 0;
      wpos = 0;
    }

  let channels t = channels t.buffer

  let read_space t =
    if t.wpos >= t.rpos then t.wpos - t.rpos else t.size - (t.rpos - t.wpos)

  let write_space t =
    if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
    else t.rpos - t.wpos - 1

  let read_advance t n =
    assert (n <= read_space t);
    if t.rpos + n < t.size then t.rpos <- t.rpos + n
    else t.rpos <- t.rpos + n - t.size

  let write_advance t n =
    assert (n <= write_space t);
    if t.wpos + n < t.size then t.wpos <- t.wpos + n
    else t.wpos <- t.wpos + n - t.size

  let peek t buf =
    let len = length buf in
    assert (len <= read_space t);
    let pre = t.size - t.rpos in
    let extra = len - pre in
    if extra > 0 then (
      blit t.buffer t.rpos buf 0 pre;
      blit t.buffer 0 buf pre extra)
    else blit t.buffer t.rpos buf 0 len

  let read t buf =
    peek t buf;
    read_advance t (length buf)

  let write t buf =
    let len = length buf in
    assert (len <= write_space t);
    let pre = t.size - t.wpos in
    let extra = len - pre in
    if extra > 0 then (
      blit buf 0 t.buffer t.wpos pre;
      blit buf pre t.buffer 0 extra)
    else blit buf 0 t.buffer t.wpos len;
    write_advance t len

  let transmit t f =
    if t.wpos = t.rpos then 0
    else (
      let len0 =
        if t.wpos >= t.rpos then t.wpos - t.rpos else t.size - t.rpos
      in
      let len = f (sub t.buffer t.rpos len0) in
      assert (len <= len0);
      read_advance t len;
      len)
end

module Ringbuffer_ext = struct
  type t = { mutable ringbuffer : Ringbuffer.t }

  let prepare buf len =
    if Ringbuffer.write_space buf.ringbuffer >= len then buf.ringbuffer
    else (
      let rb =
        Ringbuffer.create
          (Ringbuffer.channels buf.ringbuffer)
          (Ringbuffer.read_space buf.ringbuffer + len)
      in
      while Ringbuffer.read_space buf.ringbuffer <> 0 do
        ignore
          (Ringbuffer.transmit buf.ringbuffer (fun buf ->
               Ringbuffer.write rb buf;
               length buf))
      done;
      buf.ringbuffer <- rb;
      rb)

  let channels rb = Ringbuffer.channels rb.ringbuffer
  let peek rb = Ringbuffer.peek rb.ringbuffer
  let read rb = Ringbuffer.read rb.ringbuffer

  let write rb buf =
    let rb = prepare rb (length buf) in
    Ringbuffer.write rb buf

  let transmit rb = Ringbuffer.transmit rb.ringbuffer
  let read_space rb = Ringbuffer.read_space rb.ringbuffer
  let write_space rb = Ringbuffer.write_space rb.ringbuffer
  let read_advance rb = Ringbuffer.read_advance rb.ringbuffer
  let write_advance rb = Ringbuffer.write_advance rb.ringbuffer
  let create chans len = { ringbuffer = Ringbuffer.create chans len }
end

module Analyze = struct
  let rms buf ofs len =
    Array.init (channels buf) (fun i -> Mono.Analyze.rms buf.(i) ofs len)

  (* See https://github.com/FFmpeg/FFmpeg/blob/master/libavfilter/af_replaygain.c *)
  (* See https://wiki.hydrogenaud.io/index.php?title=ReplayGain_specification *)

  (** Replaygain computations. *)
  module ReplayGain = struct
    type t = {
      channels : int;
      mutable frame_pos : int;
      frame_length : int;
      prefilter : float array -> float array;
      mutable peak : float;
      mutable rms : float;
      histogram : int array;
    }

    exception Not_supported

    let histogram_slots = 12000

    (** Create internal state. *)
    let create =
      let coeffs =
        [
          ( 48000,
            ( [|
                1.00000000000000;
                -3.84664617118067;
                7.81501653005538;
                -11.34170355132042;
                13.05504219327545;
                -12.28759895145294;
                9.48293806319790;
                -5.87257861775999;
                2.75465861874613;
                -0.86984376593551;
                0.13919314567432;
              |],
              [|
                0.03857599435200;
                -0.02160367184185;
                -0.00123395316851;
                -0.00009291677959;
                -0.01655260341619;
                0.02161526843274;
                -0.02074045215285;
                0.00594298065125;
                0.00306428023191;
                0.00012025322027;
                0.00288463683916;
              |],
              [| 1.00000000000000; -1.97223372919527; 0.97261396931306 |],
              [| 0.98621192462708; -1.97242384925416; 0.98621192462708 |] ) );
          ( 44100,
            ( [|
                1.00000000000000;
                -3.47845948550071;
                6.36317777566148;
                -8.54751527471874;
                9.47693607801280;
                -8.81498681370155;
                6.85401540936998;
                -4.39470996079559;
                2.19611684890774;
                -0.75104302451432;
                0.13149317958808;
              |],
              [|
                0.05418656406430;
                -0.02911007808948;
                -0.00848709379851;
                -0.00851165645469;
                -0.00834990904936;
                0.02245293253339;
                -0.02596338512915;
                0.01624864962975;
                -0.00240879051584;
                0.00674613682247;
                -0.00187763777362;
              |],
              [| 1.00000000000000; -1.96977855582618; 0.97022847566350 |],
              [| 0.98500175787242; -1.97000351574484; 0.98500175787242 |] ) );
          ( 22050,
            ( [|
                1.00000000000000;
                -1.49858979367799;
                0.87350271418188;
                0.12205022308084;
                -0.80774944671438;
                0.47854794562326;
                -0.12453458140019;
                -0.04067510197014;
                0.08333755284107;
                -0.04237348025746;
                0.02977207319925;
              |],
              [|
                0.33642304856132;
                -0.25572241425570;
                -0.11828570177555;
                0.11921148675203;
                -0.07834489609479;
                -0.00469977914380;
                -0.00589500224440;
                0.05724228140351;
                0.00832043980773;
                -0.01635381384540;
                -0.01760176568150;
              |],
              [| 1.00000000000000; -1.94561023566527; 0.94705070426118 |],
              [| 0.97316523498161; -1.94633046996323; 0.97316523498161 |] ) );
        ]
      in
      fun ~channels ~samplerate ->
        (* Frame length in samples (a frame is 50 ms). *)
        let frame_length = samplerate * 50 / 1000 in
        (* Coefficients of the Yulewalk and Butterworth filters. *)
        let yule_a, yule_b, butter_a, butter_b =
          match List.assoc_opt samplerate coeffs with
            | Some c -> c
            | None -> raise Not_supported
        in
        let yulewalk =
          Array.init channels (fun _ -> Sample.iir yule_a yule_b)
        in
        let butterworth =
          Array.init channels (fun _ -> Sample.iir butter_a butter_b)
        in
        let prefilter x =
          Array.mapi (fun i x -> x |> yulewalk.(i) |> butterworth.(i)) x
        in
        {
          channels;
          frame_pos = 0;
          frame_length;
          prefilter;
          peak = 0.;
          rms = 0.;
          histogram = Array.make histogram_slots 0;
        }

    (** Process a sample. *)
    let process_sample rg x =
      Array.iter
        (fun x ->
          let x = abs_float x in
          if x > rg.peak then rg.peak <- x)
        x;
      let x = rg.prefilter x in
      Array.iter (fun x -> rg.rms <- rg.rms +. (x *. x)) x;
      rg.frame_pos <- rg.frame_pos + 1;
      if rg.frame_pos >= rg.frame_length then (
        (* Minimum value is about -100 dB for digital silence. The 90 dB
           offset is to compensate for the normalized float range and 3 dB is
           for stereo samples. *)
        let rms =
          (10. *. log10 (rg.rms /. float (rg.frame_length * rg.channels)))
          +. 90.
        in
        let level =
          int_of_float (100. *. rms) |> max 0 |> min (histogram_slots - 1)
        in
        rg.histogram.(level) <- rg.histogram.(level) + 1;
        rg.rms <- 0.;
        rg.frame_pos <- 0)

    (** Process a buffer. *)
    let process rg buf off len =
      assert (channels buf = rg.channels);
      for i = off to off + len - 1 do
        let x = Array.init rg.channels (fun c -> buf.(c).(i)) in
        process_sample rg x
      done

    (** Computed peak. *)
    let peak rg = rg.peak

    (** Compute gain. *)
    let gain rg =
      let windows = Array.fold_left ( + ) 0 rg.histogram in
      let i = ref (histogram_slots - 1) in
      let loud_count = ref 0 in
      (* Find i below the top 5% *)
      while !i > 0 && !loud_count * 20 < windows do
        loud_count := !loud_count + rg.histogram.(!i);
        decr i
      done;
      64.54 -. (float !i /. 100.) |> max (-24.) |> min 64.
  end
end

module Effect = struct
  class type t = object
    method process : buffer -> int -> int -> unit
  end

  class chain (e1 : t) (e2 : t) =
    object
      method process buf ofs len =
        e1#process buf ofs len;
        e2#process buf ofs len
    end

  class of_mono chans (g : unit -> Mono.Effect.t) =
    object
      val g = Array.init chans (fun _ -> g ())

      method process buf ofs len =
        for c = 0 to chans - 1 do
          g.(c)#process buf.(c) ofs len
        done
    end

  class biquad_filter chans samplerate kind ?gain freq q =
    of_mono
      chans
      (fun () ->
        (new Mono.Effect.biquad_filter samplerate kind ?gain freq q
          :> Mono.Effect.t))

  class type delay_t = object
    inherit t
    method set_delay : float -> unit
    method set_feedback : float -> unit
  end

  class delay_only chans sample_rate delay =
    let delay = int_of_float (float sample_rate *. delay) in
    object
      val mutable delay = delay
      method set_delay d = delay <- int_of_float (float sample_rate *. d)
      val rb = Ringbuffer_ext.create chans 0
      initializer Ringbuffer_ext.write rb (create chans delay)

      method process buf ofs len =
        Ringbuffer_ext.write rb (sub buf ofs len);
        Ringbuffer_ext.read rb (sub buf ofs len)
    end

  class delay chans sample_rate delay once feedback =
    let delay = int_of_float (float sample_rate *. delay) in
    object
      val mutable delay = delay
      method set_delay d = delay <- int_of_float (float sample_rate *. d)
      val mutable feedback = feedback
      method set_feedback f = feedback <- f
      val rb = Ringbuffer_ext.create chans 0
      val tmpbuf = Buffer_ext.create chans 0

      method process buf ofs len =
        if once then Ringbuffer_ext.write rb buf;
        (* Make sure that we have a past of exactly d samples. *)
        if Ringbuffer_ext.read_space rb < delay then
          Ringbuffer_ext.write rb (create chans delay);
        if Ringbuffer_ext.read_space rb > delay then
          Ringbuffer_ext.read_advance rb (Ringbuffer_ext.read_space rb - delay);
        if len > delay then add_coeff buf delay feedback buf ofs (len - delay);
        let rlen = min delay len in
        let tmpbuf = Buffer_ext.prepare tmpbuf rlen in
        Ringbuffer_ext.read rb (sub tmpbuf 0 rlen);
        add_coeff buf 0 feedback tmpbuf 0 rlen;
        if not once then Ringbuffer_ext.write rb buf
    end

  class delay_ping_pong chans sample_rate delay once feedback =
    let r1 = new delay_only 1 sample_rate delay in
    let d1 = new delay 1 sample_rate (2. *. delay) once feedback in
    let d1' = new chain (r1 :> t) (d1 :> t) in
    let d2 = new delay 1 sample_rate (2. *. delay) once feedback in
    object
      initializer assert (chans = 2)

      method set_delay d =
        r1#set_delay d;
        d1#set_delay (2. *. d);
        d2#set_delay (2. *. d)

      method set_feedback f =
        d1#set_feedback f;
        d2#set_feedback f

      method process buf ofs len =
        assert (channels buf = 2);
        (* Add original on channel 0. *)
        d1'#process [| buf.(0) |] ofs len;
        d2#process [| buf.(1) |] ofs len
    end

  let delay chans sample_rate d ?(once = false) ?(ping_pong = false) feedback =
    if ping_pong then new delay_ping_pong chans sample_rate d once feedback
    else new delay chans sample_rate d once feedback

  (* See http://www.musicdsp.org/archive.php?classid=4#169 *)
  (* times in sec, ratios in dB, gain linear *)
  class compress ?(attack = 0.1) ?(release = 0.1) ?(threshold = -10.)
    ?(ratio = 3.) ?(knee = 1.) ?(rms_window = 0.1) ?(gain = 1.) chans samplerate
    =
    (* Number of samples for computing rms. *)
    let rmsn = samples_of_seconds samplerate rms_window in
    let samplerate = float samplerate in
    object
      val mutable attack = attack
      method set_attack a = attack <- a
      val mutable release = release
      method set_release r = release <- r
      val mutable threshold = threshold
      method set_threshold t = threshold <- t
      val mutable ratio = ratio
      method set_ratio r = ratio <- r
      val mutable knee = knee
      method set_knee k = knee <- k
      val mutable gain = gain
      method set_gain g = gain <- g

      (* [rmsn] last squares. *)
      val rmsv = Array.make rmsn 0.

      (* Current position in [rmsv]. *)
      val mutable rmsp = 0

      (* Current squares of RMS. *)
      val mutable rms = 0.

      (* Processing variables. *)
      val mutable amp = 0.

      (* Envelope. *)
      val mutable env = 0.

      (* Current gain. *)
      val mutable g = 1.

      method process (buf : buffer) ofs len =
        let ratio = (ratio -. 1.) /. ratio in

        (* Attack and release "per sample decay". *)
        let g_attack =
          if attack = 0. then 0. else exp (-1. /. (samplerate *. attack))
        in
        let ef_a = g_attack *. 0.25 in
        let g_release =
          if release = 0. then 0. else exp (-1. /. (samplerate *. release))
        in
        let ef_ai = 1. -. ef_a in

        (* Knees. *)
        let knee_min = lin_of_dB (threshold -. knee) in
        let knee_max = lin_of_dB (threshold +. knee) in
        for i = 0 to len - 1 do
          (* Input level. *)
          let lev_in =
            let ans = ref 0. in
            for c = 0 to chans - 1 do
              let x = buf.(c).(i + ofs) *. gain in
              ans := !ans +. (x *. x)
            done;
            !ans /. float chans
          in

          (* RMS *)
          rms <- rms -. rmsv.(rmsp) +. lev_in;
          rms <- abs_float rms;
          (* Sometimes the rms was -0., avoid that. *)
          rmsv.(rmsp) <- lev_in;
          rmsp <- (rmsp + 1) mod rmsn;
          amp <- sqrt (rms /. float rmsn);

          (* Dynamic selection: attack or release? *)
          (* Smoothing with capacitor, envelope extraction... Here be aware of
           * pIV denormal numbers glitch. *)
          if amp > env then env <- (env *. g_attack) +. (amp *. (1. -. g_attack))
          else env <- (env *. g_release) +. (amp *. (1. -. g_release));

          (* Compute the gain. *)
          let gain_t =
            if env < knee_min then (* Do not compress. *)
              1.
            else if env < knee_max then (
              (* Knee: compress smoothly. *)
              let x = (knee +. dB_of_lin env -. threshold) /. (2. *. knee) in
              lin_of_dB (0. -. (knee *. ratio *. x *. x)))
            else
              (* Maximal (n:1) compression. *)
              lin_of_dB ((threshold -. dB_of_lin env) *. ratio)
          in
          g <- (g *. ef_a) +. (gain_t *. ef_ai);

          (* Apply the gain. *)
          let g = g *. gain in
          for c = 0 to chans - 1 do
            buf.(c).(i + ofs) <- buf.(c).(i + ofs) *. g
          done
          (*
      (* Debug messages. *)
        count <- count + 1;
        if count mod 10000 = 0 then
        self#log#f 4
        "RMS:%7.02f     Env:%7.02f     Gain: %4.02f\r%!"
        (Audio.dB_of_lin amp) (Audio.dB_of_lin env) gain
      *)
        done

      method reset =
        rms <- 0.;
        rmsp <- 0;
        for i = 0 to rmsn - 1 do
          rmsv.(i) <- 0.
        done;
        g <- 1.;
        env <- 0.;
        amp <- 0.
    end

  class auto_gain_control channels samplerate rmst (* target RMS *) rms_len
    (* duration of the RMS collection in seconds *) kup
    (* speed when volume is going up in coeff per sec *) kdown
    (* speed when volume is going down *) rms_threshold
    (* RMS threshold under which the volume should not be changed *)
      vol_init (* initial volume *) vol_min (* minimal gain *)
    vol_max (* maximal gain *) =
    let rms_len = samples_of_seconds samplerate rms_len in
    let rms_lenf = float rms_len in
    (* TODO: is this the right conversion? *)
    let kup = kup ** seconds_of_samples samplerate rms_len in
    let kdown = kdown ** seconds_of_samples samplerate rms_len in
    object
      (** Square of the currently computed rms. *)
      val mutable rms = Array.make channels 0.

      (** Number of samples collected so far. *)
      val mutable rms_collected = 0

      (** Current volume. *)
      val mutable vol = vol_init

      (** Previous value of volume. *)
      val mutable vol_old = vol_init

      (** Is it enabled? (disabled if below the threshold) *)
      val mutable enabled = true

      method process (buf : buffer) ofs len =
        for c = 0 to channels - 1 do
          let bufc = buf.(c) in
          for i = 0 to len - 1 do
            let bufci = bufc.(ofs + i) in
            if rms_collected >= rms_len then (
              let rms_cur =
                let ans = ref 0. in
                for c = 0 to channels - 1 do
                  ans := !ans +. rms.(c)
                done;
                sqrt (!ans /. float channels)
              in
              rms <- Array.make channels 0.;
              rms_collected <- 0;
              enabled <- rms_cur >= rms_threshold;
              if enabled then (
                let vol_opt = rmst /. rms_cur in
                vol_old <- vol;
                if rms_cur < rmst then vol <- vol +. (kup *. (vol_opt -. vol))
                else vol <- vol +. (kdown *. (vol_opt -. vol));
                vol <- max vol_min vol;
                vol <- min vol_max vol));
            rms.(c) <- rms.(c) +. (bufci *. bufci);
            rms_collected <- rms_collected + 1;
            (* Affine transition between vol_old and vol. *)
            bufc.(i) <-
              (vol_old +. (float rms_collected /. rms_lenf *. (vol -. vol_old)))
              *. bufci
          done
        done
    end

  (* TODO: check default parameters. *)
  let auto_gain_control channels samplerate ?(rms_target = 1.)
      ?(rms_window = 0.2) ?(kup = 0.6) ?(kdown = 0.8) ?(rms_threshold = 0.01)
      ?(volume_init = 1.) ?(volume_min = 0.1) ?(volume_max = 10.) () =
    new auto_gain_control
      channels samplerate rms_target rms_window kup kdown rms_threshold
      volume_init volume_min volume_max

  (*
  module ADSR = struct
  type t = Mono.Effect.ADSR.t

  type state = Mono.Effect.ADSR.state
  end
*)
end

module Generator = struct
  let white_noise buf ofs len =
    for c = 0 to channels buf - 1 do
      Mono.Generator.white_noise buf.(c) ofs len
    done

  class type t = object
    method set_volume : float -> unit
    method set_frequency : float -> unit
    method release : unit
    method dead : bool
    method fill : buffer -> int -> int -> unit
    method fill_add : buffer -> int -> int -> unit
  end

  class of_mono (g : Mono.Generator.t) =
    object
      val tmpbuf = Mono.Buffer_ext.create 0
      method set_volume = g#set_volume
      method set_frequency = g#set_frequency

      method fill buf ofs len =
        g#fill buf.(0) ofs len;
        for c = 1 to channels buf - 1 do
          Mono.blit buf.(0) ofs buf.(c) ofs len
        done

      method fill_add (buf : buffer) ofs len =
        let tmpbuf = Mono.Buffer_ext.prepare tmpbuf len in
        g#fill tmpbuf 0 len;
        for c = 0 to channels buf - 1 do
          Mono.add buf.(c) ofs tmpbuf 0 len
        done

      method release = g#release
      method dead = g#dead
    end

  class chain (g : t) (e : Effect.t) : t =
    object
      method fill buf ofs len =
        g#fill buf ofs len;
        e#process buf ofs len

      val tmpbuf = Buffer_ext.create 0 0

      method fill_add buf ofs len =
        let tmpbuf = Buffer_ext.prepare tmpbuf ~channels:(channels buf) len in
        g#fill tmpbuf 0 len;
        add buf ofs tmpbuf 0 len

      method set_volume = g#set_volume
      method set_frequency = g#set_frequency
      method release = g#release
      method dead = g#dead
    end
end

module IO = struct
  exception Invalid_file
  exception Invalid_operation
  exception End_of_stream

  module Reader = struct
    class type t = object
      method channels : int
      method sample_rate : int
      method length : int
      method duration : float
      method seek : int -> unit
      method close : unit
      method read : buffer -> int -> int -> int
    end

    class virtual base =
      object (self)
        method virtual channels : int
        method virtual sample_rate : int
        method virtual length : int
        method duration = float self#length /. float self#sample_rate
        (*
    method virtual seek : int -> unit

    method virtual close : unit

    method virtual read : buffer -> int -> int -> int
  *)
      end

    (* TODO: handle more formats... *)
    class virtual wav =
      object (self)
        inherit IO.helper
        method virtual private stream_close : unit
        method virtual private stream_seek : int -> unit
        method virtual private stream_cur_pos : int
        val mutable sample_rate = 0
        val mutable channels = 0

        (* Size of a sample in bits. *)
        val mutable sample_size = 0
        val mutable bytes_per_sample = 0

        (* Length in samples. *)
        val mutable length = 0
        val mutable data_offset = 0
        method sample_rate = sample_rate
        method channels = channels
        method length = length

        initializer
          if self#input 4 <> "RIFF" then
            (* failwith "Bad header: \"RIFF\" not found"; *)
            raise Invalid_file;
          (* Ignore the file size *)
          ignore (self#input 4);
          if self#input 8 <> "WAVEfmt " then
            (* failwith "Bad header: \"WAVEfmt \" not found"; *)
            raise Invalid_file;
          (* Now we always have the following uninteresting bytes:
           * 0x10 0x00 0x00 0x00 0x01 0x00 *)
          ignore (self#really_input 6);
          channels <- self#input_short;
          sample_rate <- self#input_int;
          (* byt_per_sec *) ignore self#input_int;
          (* byt_per_samp *) ignore self#input_short;
          sample_size <- self#input_short;

          let section = self#really_input 4 in
          if section <> "data" then (
            if section = "INFO" then
              (* failwith "Valid wav file but unread"; *)
              raise Invalid_file;
            (* failwith "Bad header : string \"data\" not found" *)
            raise Invalid_file);

          let len_dat = self#input_int in
          data_offset <- self#stream_cur_pos;
          bytes_per_sample <- sample_size / 8 * channels;
          length <- len_dat / bytes_per_sample

        method read (buf : buffer) ofs len =
          let sbuflen = len * channels * 2 in
          let sbuf = self#input sbuflen in
          let sbuflen = String.length sbuf in
          let len = sbuflen / (channels * 2) in
          begin
            match sample_size with
              | 16 -> S16LE.to_audio sbuf 0 buf ofs len
              | 8 -> U8.to_audio sbuf 0 buf ofs len
              | _ -> assert false
          end;
          len

        method seek n =
          let n = data_offset + (n * bytes_per_sample) in
          self#stream_seek n

        method close = self#stream_close
      end

    class of_wav_file fname =
      object
        inherit IO.Unix.rw ~read:true fname
        inherit base
        inherit wav
      end
  end

  module Writer = struct
    class type t = object
      method write : buffer -> int -> int -> unit
      method close : unit
    end

    class virtual base chans sr =
      object
        method private channels : int = chans
        method private sample_rate : int = sr
      end

    class virtual wav =
      object (self)
        inherit IO.helper
        method virtual private stream_write : string -> int -> int -> int
        method virtual private stream_seek : int -> unit
        method virtual private stream_close : unit
        method virtual private channels : int
        method virtual private sample_rate : int

        initializer
          let bits_per_sample = 16 in
          (* RIFF *)
          self#output "RIFF";
          self#output_int 0;
          self#output "WAVE";

          (* Format *)
          self#output "fmt ";
          self#output_int 16;
          self#output_short 1;
          self#output_short self#channels;
          self#output_int self#sample_rate;
          self#output_int
            (self#sample_rate * self#channels * bits_per_sample / 8);
          self#output_short (self#channels * bits_per_sample / 8);
          self#output_short bits_per_sample;

          (* Data *)
          self#output "data";
          (* size of the data, to be updated afterwards *)
          self#output_short 0xffff;
          self#output_short 0xffff

        val mutable datalen = 0

        method write buf ofs len =
          let s = S16LE.make buf ofs len in
          self#output s;
          datalen <- datalen + String.length s

        method close =
          self#stream_seek 4;
          self#output_int (36 + datalen);
          self#stream_seek 40;
          self#output_int datalen;
          self#stream_close
      end

    class to_wav_file chans sr fname =
      object
        inherit base chans sr
        inherit IO.Unix.rw ~write:true fname
        inherit wav
      end
  end

  module RW = struct
    class type t = object
      method read : buffer -> int -> int -> unit
      method write : buffer -> int -> int -> unit
      method close : unit
    end

    class virtual bufferized channels ~min_duration ~fill_duration ~max_duration
      ~drop_duration =
      object
        method virtual io_read : buffer -> unit
        method virtual io_write : buffer -> unit

        initializer
          assert (fill_duration <= max_duration);
          assert (drop_duration <= max_duration)

        val rb = Ringbuffer.create channels max_duration

        method read buf =
          let len = length buf in
          let rs = Ringbuffer.read_space rb in
          if rs < min_duration + len then (
            let ps = min_duration + len - rs in
            Ringbuffer.write rb (create channels ps));
          Ringbuffer.read rb buf

        method write buf =
          let len = length buf in
          let ws = Ringbuffer.write_space rb in
          if ws + len > max_duration then
            Ringbuffer.read_advance rb (ws - drop_duration);
          Ringbuffer.write rb buf
      end
  end
end
