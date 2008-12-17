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

external caml_float_pcm_convert_le_byte : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_le_byte" "caml_float_pcm_convert_le_native"

external caml_float_pcm_convert_be_byte : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_be_byte" "caml_float_pcm_convert_be_native"

let resample_s16le
      src src_off len signed samplesize big_endian
      ratio dst dst_off =
  if big_endian then
    caml_float_pcm_convert_be_byte
      src src_off len signed samplesize big_endian
      ratio dst dst_off
  else
    caml_float_pcm_convert_le_byte
      src src_off len signed samplesize big_endian
      ratio dst dst_off

external to_s16le : float array array -> int -> int -> string -> int -> int
         = "caml_float_pcm_to_s16le"

let to_s16le_ni buf ofs len dst dst_ofs =
  let ans = ref 0 in
    for c = 0 to Array.length buf - 1 do
      ans := to_s16le [|buf.(c)|] ofs len dst.(c) dst_ofs;
    done;
    !ans

external from_s16le : float array array -> int -> string -> int -> int -> unit
         = "caml_float_pcm_from_s16le"

let from_s16le_ni dbuf dofs buf ofs len =
  for c = 0 to Array.length buf - 1 do
    from_s16le [|dbuf.(c)|] dofs buf.(c) ofs len
  done

external float_blit : float array -> int -> float array -> int -> int -> unit
     = "caml_float_array_blit"

let native_resample ratio inbuf offs len =
  if ratio = 1. then
        let outbuf = Array.make len 0. in
          float_blit inbuf offs outbuf 0 len;
          outbuf
  else
    let outlen = int_of_float (float len *. ratio) in
    let outbuf = Array.make outlen 0. in
      for i = 0 to outlen - 1 do
        let inidx = min (int_of_float (float i /. ratio)) (len - 1) in
          outbuf.(i) <- inbuf.(inidx + offs)
      done;
      outbuf

let get_float_pcm b =
  let tracks = Array.to_list (Frame.get_tracks b) in
  let ans =
    List.fold_left
      (fun l t ->
         match t with
           | Frame.Float_pcm (_,a) -> a::l
           | _ -> l
      ) [] tracks in
    Array.of_list ans

(** Accumulate float PCM and generate float_pcm tracks. *)
module Generator =
struct
  type t = {
    (* Format *)
    out_freq  : int;
    out_chans : int;
    resample  : float -> float array array -> int -> int -> float array array;
    (* Accumulated input data. *)
    mutable metadata : (int*Frame.metadata) list ; (* ticks   *)
    mutable breaks   : int list ;                  (* ticks   *)
    mutable length   : int ;                       (* samples *)
    mutable offset   : int ; (* offset in samples the first buffer *)
    mutable buffers  : float array array Queue.t ;
  }

  let create ?(out_freq = Fmt.samples_per_second())
             ?(out_chans = Fmt.channels()) () =
    let conv = Audio_converter.Samplerate.create out_chans in
      {
        out_freq  = out_freq ;
        out_chans = out_chans ;
        metadata  = [] ;
        breaks    = [] ;
        length    = 0 ;
        offset    = 0 ;
        buffers   = Queue.create () ;
        resample  = Audio_converter.Samplerate.resample conv ;
      }

  let length b = b.length

  let remaining abg =
    match abg.breaks with
      | a :: _ -> a
      | _ -> Fmt.ticks_of_samples abg.length

  let clear abg =
    abg.length <- 0;
    abg.offset <- 0;
    abg.buffers <- Queue.create ();
    abg.metadata <- [];
    abg.breaks <- []

  let feed abg ?(sample_freq = Fmt.samples_per_second ()) buf =
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
    let buf =
      abg.resample (float abg.out_freq /. float sample_freq) buf
                   0 (Array.length buf.(0))
    in
      abg.length <- abg.length + (Array.length buf.(0)) ;
      Queue.add buf abg.buffers

  let add_metadata abg m =
    abg.metadata <- abg.metadata @ [Fmt.ticks_of_samples abg.length, m]

  let add_break abg =
    abg.breaks <- abg.breaks @ [Fmt.ticks_of_samples abg.length]

  (* Take all data from a frame: breaks, metadata and available audio. *)
  let feed_from_frame abg frame =
    let size = Frame.size frame in
    let samples = Fmt.samples_of_ticks (Frame.position frame) in
      abg.metadata <-
        abg.metadata @
          (List.map
             (fun (p,m) -> Fmt.ticks_of_samples abg.length + p, m)
             (Frame.get_all_metadata frame)) ;
      abg.breaks <-
        abg.breaks @
          (List.map
             (fun p -> Fmt.ticks_of_samples abg.length + p)
             (List.filter (fun x -> x < size) (Frame.breaks frame))) ;
      feed abg ~sample_freq:(Fmt.samples_per_second ())
        (Array.map
           (fun x -> Array.sub x 0 samples)
           (get_float_pcm frame))

  (* Advance metadata and breaks by [pos] samples. *)
  let advance abg pos =
    let pos = Fmt.ticks_of_samples pos in
    let meta = List.map (fun (x,y) -> (x-pos,y)) abg.metadata in
    let breaks = List.map (fun x -> x-pos) abg.breaks in
      abg.metadata <- List.filter (fun x -> fst x >= 0) meta;
      abg.breaks <- List.filter (fun x -> x >= 0) breaks

  (** Remove [len] samples of data. *)
  let rec remove abg len =
    assert (abg.length >= len) ;
    if len>0 then
    let b = Queue.peek abg.buffers in
      (* Is it enough to advance in the first buffer?
       * Or do we need to consume it completely and go farther in the queue? *)
      if abg.offset + len < Array.length b.(0) then begin
        abg.length <- abg.length - len ;
        abg.offset <- abg.offset + len ;
        advance abg len
      end else
        let removed = Array.length b.(0) - abg.offset in
          ignore (Queue.take abg.buffers) ;
          abg.length <- abg.length - removed ;
          abg.offset <- 0 ;
          advance abg removed ;
          remove abg (len-removed)

  (* Fill the frame from the generator's data. *)
  let fill abg frame =
    (* Audio only (for now) so the official unit is the sample. *)
    let buf = get_float_pcm frame in
    let offset = Fmt.samples_of_ticks (Frame.position frame) in
    let buffer_size = Array.length buf.(0) in
    let blit src src_off dst dst_off len =
      for c = 0 to Array.length src - 1 do
        float_blit src.(c) src_off dst.(c) dst_off len
      done
    in
    (* The main loop takes the current offset in the output buffer,
     * and iterates on input buffer chunks. *)
    let rec aux offset =
      (* How much (more) data should be output? *)
      let needed =
        min
          (Fmt.samples_of_ticks (remaining abg))
          (buffer_size - offset)
      in
      let offset_ticks = Fmt.ticks_of_samples offset in
        if needed = 0 then begin
          Frame.add_break frame offset_ticks ;
          if Frame.is_partial frame then
            match abg.breaks with
              | 0::tl -> abg.breaks <- tl
              | [] -> () (* end of stream / underrun ... *)
              | _ -> assert false
        end else
          let block = Queue.peek abg.buffers in
          let blocklen = Array.length block.(0) - abg.offset in
          let copied = min needed blocklen in
            blit
              block abg.offset
              buf offset
              copied ;
            List.iter
              (fun (p,m) ->
                 if p < Fmt.ticks_of_samples copied then
                   Frame.set_metadata frame (p + offset_ticks) m)
              abg.metadata ;
            advance abg copied ;
            (* Update buffer data -- did we consume a full block? *)
            if blocklen <= needed then begin
              ignore (Queue.take abg.buffers) ;
              abg.length <- abg.length - blocklen ;
              abg.offset <- 0
            end else begin
              abg.length <- abg.length - needed ;
              abg.offset <- abg.offset + needed
            end ;
            (* Add more data by recursing on the next block, or finish. *)
            if blocklen < needed then
              aux (offset+blocklen)
            else begin
              Frame.add_break frame (Fmt.ticks_of_samples (offset+needed)) ;
              if Frame.is_partial frame then
                match abg.breaks with
                  | 0::tl -> abg.breaks <- tl
                  | [] -> () (* end of stream / underrun ... *)
                  | _ -> assert false
            end
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

  let create ~channels ~samplesize ~signed ~big_endian ~in_freq ~out_freq =
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

  let feed g b =
    Generator.feed g.generator ~sample_freq:(g.in_freq) (g.convert b)

  let add_metadata g x = Generator.add_metadata g.generator x

  let add_break g = Generator.add_break g.generator

  let length g = Generator.length g.generator

  let remaining g = Generator.remaining g.generator

  let remove g = Generator.remove g.generator

  let fill g = Generator.fill g.generator
end

(* Sound processing *)

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
