(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(**
  * Functions for manipulating audio buffers and buffer generators.
  * @author Samuel Mimram and David Baelde
  *)

(* Warning: critical parameter.
 * Don't forget to change it in rtp_c.c too.
 * Also, noblank.ml relies on that value to be not too excessive,
 * otherwise some integers would be too small too store intensity. *)
let audio_buffer_size = 1024*4

let _ = Callback.register "mixer_audio_buffer_size" audio_buffer_size

type format =
    {
      channels : int;
      sample_freq : int;
      sample_size : int;
      big_endian : bool;
      signed : bool
    }

module Generator =
struct

  type t =
      {
        mutable length : int ;
        mutable offset : int ;
        mutable buffers : string Queue.t
      }

  let create () = { length=0 ; offset=0 ; buffers=Queue.create () }

  let length abg = abg.length

  exception Invalid_format
  let _ =
    Callback.register_exception "mixer_exn_invalid_format" Invalid_format
  external convert_format : format -> string -> string =
    "ocaml_mixer_convert_format"

  let feed abg fmt buf =
    let buf = convert_format fmt buf in
    abg.length <- abg.length + (String.length buf) ;
    Queue.add buf abg.buffers

  let rec remove abg len =
    assert (abg.length >= len) ;
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

  let should_be_feeded abg =
    abg.length < audio_buffer_size

  let is_empty abg = abg.length = 0

  let fill abg buf offset =
    let rec aux offset =
      let needed = audio_buffer_size - offset in
        if abg.length > 0 && needed > 0
        then
          (* Can we fill ? Do we need to fill ? *)
          begin
            let block = Queue.peek abg.buffers in
            let blocklen = String.length block - abg.offset in
            let more =
              if blocklen <= needed then
                begin
                  (* Here we consume the full block *)
                  String.blit
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
                  String.blit
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
      try aux offset with
        | Failure "String.blit" -> failwith "Mixer.Generator.fill"

end

module Buffer =
struct

  type metadata = (string,string) Hashtbl.t

  type t = {
    mutable buffer : string ;
    (* End of track markers.
     * However, a break at the end of the buffer is not an end of track.
     * So maybe we should rather call that a end-of-fill marker,
     * and notice that end-of-fills in the middle of a buffer are end-of-tracks.
     * If needed, the breaks needs to be put at the beginning of next track. *)
    mutable breaks : int list ;
    (* Metadatas can be put anywhere in the stream *)
    mutable metadata : (int*metadata) list ;
  }

  let format = {
    channels = 2;
    sample_freq = 44100;
    sample_size = 16;
    big_endian = false;
    signed = true
  }

  let size = audio_buffer_size
  let length = (float audio_buffer_size)/.(44100.*.2.*.2.)

  let create () = {
    buffer = String.make size '\000' ;
    breaks = [] ;
    metadata = [] ;
  }

  let position ab =
    match ab.breaks with
    | [] -> 0
    | a::_ -> a

  let is_partial ab = position ab < audio_buffer_size
  let free ab = ab.breaks <- [] ; ab.metadata <- []

  let breaks ab = ab.breaks
  let set_breaks ab breaks = ab.breaks <- breaks
  let add_break ab b = ab.breaks <- b::ab.breaks

  (** Metadata stuff *)

  exception No_metadata
  let free_metadata ab = ab.metadata <- []
  let set_metadata ab t m = ab.metadata <- (t,m)::ab.metadata
  let get_metadata ab t =
    try
      Some (List.assoc t ab.metadata)
    with Not_found -> None
  let get_all_metadata ab = ab.metadata
  let set_all_metadata ab l = ab.metadata <- l

  (* Fill *)

  exception No_chunk
  (* Get the (end of) next chunk from [from] *)
  let get_chunk ab from =
    assert (is_partial ab) ;
    let p = position ab in
    let rec aux foffset f =
      (* We always have p >= foffset *)
      match f with
      | [] -> raise No_chunk
      | i::tl ->
          (* Breaks are between bytes, they do range from 0 to size. *)
          assert (0<=i && i<=size) ;
          if i = 0 && ab.breaks = [] then
            (* The only empty track that we copy,
             * trying to copy empty tracks in the middle could be useful
             * for packets like those forged by add, with a fake first break,
             * but isn't needed (yet) and is painful to implement. *)
            add_break ab 0
          else if foffset < i && i > p then begin
            String.blit from.buffer p ab.buffer p (i-p) ;
            add_break ab i ;
            List.iter
              (fun (mp,m) ->
                 (* Copy new metadata blocks for this chunk.
                  * We exclude blocks at the end of chunk, cause they're
                  * useless and that would lead to multiple copies. *)
                 if p<=mp && mp<i then
                   set_metadata ab mp m)
              from.metadata
          end else
            aux i tl
    in
      aux 0 (List.rev from.breaks)

  let fill ab abg =
    let a = Generator.fill abg ab.buffer (position ab) in
      add_break ab a

  (* Check that there's no empty track.
   * Optional [base] defaults to 0. It ignores empty track at [base],
   * it's useful for forged buffers when you force the starting point. *)
  let ill ?(base=0) ab =
    let rec aux =
      function
      | [] | [_] -> false
      | o::oo::l ->
          assert (o>=oo) ;
          if o=oo && o<>base then true else aux (oo::l)
    in
      aux ab.breaks

  (** Direct audio processing *)

  let to_string ab = ab.buffer

  let blankify ab off len =
    String.fill ab.buffer off len '\000'

  exception Invalid_argument
  let _ =
    Callback.register_exception "mixer_exn_invalid_argument" Invalid_argument

  external add_buffer : string -> int -> string -> int -> int -> unit =
    "ocaml_mixer_add_buffer"
  external change_volume : string -> int -> int -> float -> unit =
    "ocaml_mixer_change_volume"
  external sine : string -> int -> int -> int -> float -> float =
    "ocaml_mixer_sine"
  external rms  : string -> float =
    "ocaml_mixer_rms"

  let change_volume buf off len f =
    assert (off mod 2 = 0 && len mod 2 = 0) ;
    change_volume buf.buffer off len f

  let add buf1 off1 buf2 off2 len =
    assert (off1 mod 2 = 0 && off2 mod 2 = 0 && len mod 2 = 0) ;
    add_buffer buf1.buffer off1 buf2.buffer off2 len

  type filter_type = Low_pass | High_pass | Band_pass | Notch

  external simple_filter :
             string -> int -> int -> int -> float -> filter_type -> unit =
    "ocaml_mixer_simple_filter_bytecode" "ocaml_mixer_simple_filter"

  let simple_filter buf off len =
    assert (len mod 2 = 0 && off mod 2 = 0) ;
    simple_filter buf.buffer off len

  let sine buf off len freq phi =
    assert (off mod 2 = 0 && len mod 2 = 0) ;
    sine buf.buffer off len freq phi

  let rms buf =
    rms buf.buffer
end
