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

(** Generic file decoder. *)

module Generator = Generator.From_audio_video

type 'a file_decoder =
  {
    log      : Dtools.Log.t;
    openfile : string -> 'a;
    get_kind : 'a -> Frame.content_kind;
    decode   : 'a -> Generator.t -> unit;
    position : 'a -> int; (* in bytes *)
    close    : 'a -> unit
  }

let decode decoder mode file =
  let fd =
    decoder.log#f 5 "Open %S." file ;
    try
      decoder.openfile file
    with
      | e ->
         decoder.log#f 5 "Could not decode file." ;
         raise e
  in
  let gen = Generator.create mode in
  let buffer_length = Lazy.force Frame.size in
  let file_size = (Unix.stat file).Unix.st_size in
  let out_ticks = ref 0 in
  let closed = ref false in

  let close () =
    assert (not !closed) ;
    closed := true ;
    decoder.log#f 5 "Close %S." file ;
    decoder.close fd
  in

  let fill frame =
    assert (not !closed) ;

    begin
      try
        while Generator.length gen < buffer_length do
          decoder.decode fd gen
        done
      with
        | _ -> ()
    end ;

    let offset = Frame.position frame in
      Generator.fill gen frame ;
      let in_bytes = decoder.position fd in
      let gen_len = Generator.length gen in
        out_ticks := !out_ticks + Frame.position frame - offset ;
        (* Compute an estimated number of remaining ticks. *)
        if in_bytes = 0 then -1 else
          let compression =
            (float (!out_ticks+gen_len)) /. (float in_bytes)
          in
          let remaining_ticks =
            (float gen_len) +.
            (float (file_size - in_bytes)) *. compression
          in
            int_of_float remaining_ticks
  in
    { Decoder.fill = fill ; Decoder.close = close }

(** Top-level wrapper to also check content_type *)
let decode decoder name kind =
  try
    let content_kind =
      let data = decoder.openfile name in
      let content_kind =
        try
          decoder.get_kind data
        with
          | e -> decoder.close data; raise e
      in
        decoder.close data;
        content_kind
    in
      if Frame.kind_sub_kind kind content_kind then
        let mode = 
          match content_kind.Frame.audio, 
                content_kind.Frame.video 
          with
            | Frame.Zero, Frame.Succ _   -> Generator.Video
            | Frame.Succ _, Frame.Zero   -> Generator.Audio
            | Frame.Succ _, Frame.Succ _ -> Generator.Both
            | _,_                        -> assert false
        in
        Some (decode decoder mode name)
      else
        None
  with _ -> None
