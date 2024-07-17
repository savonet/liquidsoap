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

let create_buffer chans len =
  Array.init chans (fun _ -> Array.make len 0.)

let sub buf ofs len =
  if ofs = 0 && len = Array.length buf then
    buf
  else
    Array.map (fun a -> Array.sub a ofs len) buf

external resample_s16le : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_byte" "caml_float_pcm_convert_native"

external to_s16le : float array array -> int -> int -> string -> int -> int = "caml_float_pcm_to_s16le"

let to_s16le_ni buf ofs len dst dst_ofs =
  let ans = ref 0 in
    for c = 0 to Array.length buf - 1 do
      ans := to_s16le [|buf.(c)|] ofs len dst.(c) dst_ofs;
    done;
    !ans

external float_blit : float array -> int -> float array -> int -> int -> unit
     = "caml_float_array_blit"

let resample ratio inbuf offs len =
  if ratio = 1. then
    if offs = 0 && len = Array.length inbuf then
      inbuf (* Array.copy inbuf (* TODO: can we return inbuf itself? *) *)
    else
      (
        let outbuf = Array.make len 0. in
          float_blit inbuf offs outbuf 0 len;
          outbuf
      )
  else
    let outlen = int_of_float (float len *. ratio) in
    let outbuf = Array.make outlen 0. in
      for i = 0 to outlen - 1 do
        let inidx = min (int_of_float (float i /. ratio)) (len - 1) in
          outbuf.(i) <- inbuf.(inidx + offs)
      done;
      outbuf

let resample =
  match Configure.resample with
    | Some f -> f
    | None -> resample

module Channel =
struct
  let resample = resample
end

(** Accumulate float PCM and generate float_pcm tracks. *)
module Generator =
struct
  type t = {
    (* Format *)
    out_freq  : int;
    out_chans : int;
    (* Accumulated input data. *)
    mutable length  : int ;
    mutable offset  : int ; (* offset in the first array of buffers *)
    mutable buffers : float array array Queue.t ;
  }

  let create ?(out_freq = Fmt.samples_per_second()) ?(out_chans = Fmt.channels()) () = {
    out_freq=out_freq;
    out_chans=out_chans;
    length=0 ;
    offset=0 ;
    buffers=Queue.create () ;
  }

  let length b = b.length

  let clear abg =
    abg.length <- 0;
    abg.offset <- 0;
    abg.buffers <- Queue.create ()

  let resample infreq outfreq a =
    if infreq = outfreq then a
    else
      let ratio = float outfreq /. float infreq in
        resample ratio a 0 (Array.length a)

  let feed abg ?(sample_freq = Fmt.samples_per_second()) buf =
    let buf =
      match Array.length buf with
        | n when n = Fmt.channels () -> buf
        | n when n > Fmt.channels () ->
            (* TODO: is this really what we want here? *)
            Array.sub buf 0 (Fmt.channels ())
        | 1 -> Array.make (Fmt.channels ()) buf.(0)
        | 0 -> failwith "Generator.feed: no channels"
        | _ -> failwith "Generator.feed: not enough channels"
    in
    let buf = Array.map (resample sample_freq abg.out_freq) buf in
      abg.length <- abg.length + (Array.length buf.(0)) ;
      Queue.add buf abg.buffers

  let is_empty abg = abg.length = 0

  (** Remove [len] bytes of input. *)
  let rec remove abg len =
    assert (abg.length >= len) ;
    if len>0 then
    let b = Queue.peek abg.buffers in
      if abg.offset + len < Array.length b.(0) then begin
        abg.length <- abg.length - len ;
        abg.offset <- abg.offset + len
      end else begin
        (* We first remove (String.length b) - abg.offset *)
        ignore (Queue.take abg.buffers) ;
        abg.length <- abg.length - (Array.length b.(0)) + abg.offset ;
        abg.offset <- 0 ;
        (* And then remove the remainder *)
        remove abg (len - (Array.length b.(0)) + abg.offset)
      end

  (** Fill the float array array [buf] starting at [offset]. *)
  let fill abg buf offset =
    let buffer_size = Array.length buf.(0) in
    let blit src src_off dst dst_off len =
      for c = 0 to Array.length src - 1 do
        float_blit src.(c) src_off dst.(c) dst_off len
      done
    in
    let rec aux offset =
      let needed = buffer_size - offset in
        if abg.length > 0 && needed > 0
        then
          (* Can we fill ? Do we need to fill ? *)
          begin
            let block = Queue.peek abg.buffers in
            let blocklen = Array.length block.(0) - abg.offset in
            let more =
              if blocklen <= needed then
                begin
                  (* Here we consume the full block *)
                  blit
                    block abg.offset
                    buf offset
                    blocklen ;
                  abg.length <- abg.length - blocklen ;
                  ignore (Queue.take abg.buffers) ;
                  abg.offset <- 0 ;
                  blocklen
                end
              else
                begin
                  (* .. there we don't need the whole block *)
                  blit
                    block abg.offset
                    buf offset
                    needed ;
                  abg.length <- abg.length - needed ;
                  abg.offset <- abg.offset + needed ;
                  needed
                end
            in
              aux (offset+more)
          end
        else
          offset
    in
      aux offset

end

(** Accumulate raw PCM and generate float_pcm tracks. *)
module Generator_from_raw =
struct
  type t =
      {
        generator : Generator.t;
        convert : string -> float array array;
        in_freq :int;
      }

  let create ~channels ~samplesize ~signed ~big_endian ~in_freq ~samples ~out_freq =
    let convert src src_off len ratio =
      let dst =
        (* TODO: convert channel number? *)
        Array.init channels (fun _ -> Array.make len 0.)
      in
        ignore
          (
            resample_s16le
              src src_off len signed samplesize big_endian
              ratio dst 0
          );
        dst
    in
    let convert src =
      convert src 0 (String.length src / (2 * channels)) 1.
    in
      {
        generator = Generator.create ~out_freq:(int_of_float out_freq) ();
        convert = convert;
        in_freq = int_of_float in_freq;
      }

  let clear g = Generator.clear g.generator

  let feed g b = Generator.feed g.generator ~sample_freq:(g.in_freq) (g.convert b)

  let length g = Generator.length g.generator

  let is_empty g = Generator.is_empty g.generator

  let remove g = Generator.remove g.generator

  let fill g = Generator.fill g.generator
end

(** Optimized structure for converting float pcm to s16le strings,
  * supporting resampling and conversions of channel numbers. *)
module To_s16le =
struct
  type t = string

  let create ~in_channels ~in_samplerate ~out_channels ~out_samplerate max =
    assert (in_channels = out_channels) ;
    assert (in_samplerate = out_samplerate) ;
    String.create (max * in_channels * 2)

  let get_output_buffer s = s

  let convert s input off len =
    to_s16le input off len s 0

end

external from_s16le : float array array -> int -> string -> int -> int -> unit = "caml_float_pcm_from_s16le"

let from_s16le_ni dbuf dofs buf ofs len =
  for c = 0 to Array.length buf - 1 do
    from_s16le [|dbuf.(c)|] dofs buf.(c) ofs len
  done

(* Sound processing *)

let resample x buf off len =
  Array.map (fun b -> resample x b off len) buf

let multiply a off len c =
  for i = 0 to Array.length a - 1 do
    for j = off to off + len - 1 do
      a.(i).(j) <- c *. a.(i).(j)
    done
  done

let blankify a off len =
  for i = 0 to Array.length a - 1 do
    for j = off to off + len - 1 do
      a.(i).(j) <- 0.
    done
  done

let add dst dst_off src src_off len =
  for i = 0 to Array.length dst - 1 do
    for j = 0 to len - 1 do
      dst.(i).(dst_off+j) <- dst.(i).(dst_off+j) +. src.(i).(src_off+j)
    done
  done

let substract y y_off x x_off len =
  for i = 0 to Array.length y - 1 do
    for j = 0 to len - 1 do
      y.(i).(y_off+j) <- y.(i).(y_off+j) -. x.(i).(x_off+j)
    done
  done

let rms a off len =
  let ans = Array.create (Array.length a) 0. in
    for c = 0 to Array.length a - 1 do
      let a_c = a.(c) in
        for i = off to off + len - 1 do
          ans.(c) <- ans.(c) +. a_c.(i) *. a_c.(i)
        done;
      ans.(c) <- sqrt (ans.(c) /. (float len))
    done;
    ans
