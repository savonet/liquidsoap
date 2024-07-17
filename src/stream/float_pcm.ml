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

external resample_s16le : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_byte" "caml_float_pcm_convert_native"

external to_s16le : float array array -> int -> int -> string -> int -> int = "caml_float_pcm_to_s16le"

external float_blit : float array -> int -> float array -> int -> int -> unit
     = "caml_float_array_blit"

let resample ratio inbuf offs len =
  if ratio = 1. then
    if offs = 0 && len = Array.length inbuf then
      Array.copy inbuf (* TODO: can we return inbuf itself? *)
    else
      (
        let outbuf = Array.make len 0. in
          Array.blit inbuf offs outbuf 0 len;
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
      (* TODO Use Fmt.channels *)
      match Array.length buf with
        | 2 -> buf
        | 1 -> [|buf.(0); buf.(0)|]
        | 0 -> failwith "Generator.feed: no channels"
        | _ -> [|buf.(0); buf.(1)|]
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

  type t = {
    (* Input format *)
    channels   : int ;
    samplesize : int ;
    signed     : bool ;
    big_endian : bool ;
    in_freq    : float ;

    (* Output format *)
    out_freq : float ;

    (* Accumulated input data. *)
    mutable buffers : string Queue.t ;
    mutable length  : int ; (* bytes *)
    mutable offset  : int ; (* bytes *)

    (* Number of needed input samples. *)
    in_samples : int
  }

  let create ~channels ~samplesize ~signed ~big_endian ~in_freq
        ~samples ~out_freq =
    if samplesize <> 16 || not signed || big_endian then
      raise (Invalid_argument "Generator_from_raw.create") ;
    {
      channels = channels ;
      samplesize = samplesize ; signed = signed ; big_endian = big_endian ;
      in_freq = in_freq ; out_freq = out_freq ;
      length = 0 ; offset = 0 ; buffers = Queue.create () ;
      in_samples = 
        int_of_float (ceil (float samples *. in_freq /. out_freq))
    }

  let clear abg =
    abg.length <- 0;
    abg.offset <- 0;
    abg.buffers <- Queue.create ()

  let feed abg buf =
    abg.length <- abg.length + (String.length buf) ;
    Queue.add buf abg.buffers

  let should_be_feeded g =
    g.in_samples > g.length * 8 / (g.channels * g.samplesize)

  let is_empty abg = abg.length = 0

  (** Length of available output data in samples. *)
  let length abg =
    let in_samples = abg.length * 8 / abg.samplesize / abg.channels in
      int_of_float ((float in_samples) *. abg.out_freq /. abg.in_freq)

  (** Remove [len] bytes of input. *)
  let rec remove abg len =
    assert (abg.length >= len) ;
    if len>0 then
    let b = Queue.peek abg.buffers in
      if abg.offset + len < String.length b then begin
        abg.length <- abg.length - len ;
        abg.offset <- abg.offset + len
      end else begin
        (* We first remove (String.length b) - abg.offset *)
        ignore (Queue.take abg.buffers) ;
        abg.length <- abg.length - (String.length b) + abg.offset ;
        abg.offset <- 0 ;
        (* And then remove the remainder *)
        remove abg (len - (String.length b) + abg.offset)
      end

  (** Fill the float array array [buf] starting at [offset]. *)
  let fill abg buf offset =
    let buffer_size = Array.length buf.(0) in
    let ratio = abg.out_freq /. abg.in_freq in
    let blit src src_off dst dst_off len =
      let len = len * 8 / abg.channels / abg.samplesize in
        resample_s16le
          src src_off len abg.signed abg.samplesize abg.big_endian
          ratio dst dst_off
    in
    let rec aux offset =
      (* How many samples are needed for the output ? *)
      let needed = buffer_size - offset in
      (* How many samples do we need to consume from the input ? *)
      let needed =
        (* TODO ceil/floor/what? *)
        int_of_float (ceil (float needed /. ratio))
      in
      (* How many bytes do we need to consume from the input ? *)
      let needed = needed * abg.channels * abg.samplesize / 8 in

        if abg.length = 0 || needed = 0 then offset else
          (* Can we fill ? Do we need to fill ? *)
          let block = Queue.peek abg.buffers in
          let blocklen = String.length block - abg.offset in
          let new_offset =
            if blocklen <= needed then begin
              (* Here we consume the full block *)
              let new_offset =
                blit
                  block abg.offset
                  buf offset
                  blocklen
              in
                abg.length <- abg.length - blocklen ;
                ignore (Queue.take abg.buffers) ;
                abg.offset <- 0 ;
                new_offset
            end else begin
              (* .. there we don't need the whole block *)
              let new_offset =
                blit
                  block abg.offset
                  buf offset
                  needed
              in
                abg.length <- abg.length - needed ;
                abg.offset <- abg.offset + needed ;
                new_offset
            end
          in
            aux new_offset
    in
      aux offset

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

let rms a off len =
  let ans = Array.create (Array.length a) 0. in
    for c = 0 to Array.length a - 1 do
      for i = off to off + len - 1 do
        ans.(c) <- ans.(c) +. a.(c).(i)*.a.(c).(i)
      done;
      ans.(c) <- sqrt (ans.(c) /. (float len))
    done;
    ans
