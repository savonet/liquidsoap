(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(** Media decoding infrastructure.
  *
  * We treat files and streams.
  * We separate detection from the actual decoding.
  * For files, the decoder detection function is passed a filename and
  * an expected content kind.
  * For streams, it is passed a MIME type and a content kind.
  *
  * In practice, most file decoders will be based on stream decoders,
  * with a specific (more precise) detection function. Although
  * we cannot force it at this point, we provide some infrastructure
  * to help.
  *
  * In the short term, the plug infrastructure should provide
  * a way to ban / prioritize
  * plugins. For example:
  *   - choose ogg_demuxer when extension = ogg
  *   - choose mad when extension = mp3
  *   - choose mad when mime-type = audio/mp3
  *
  * A few comments about the changes from the old decoding infrastructure:
  *
  * We need to change the closing policy (necessary to release resources
  * immediately). In particular stream decoders shouldn't be in charge
  * of closing the stream (sink) since they didn't open it: this trivializes
  * the stream interface (sink) to an input function and a THREAD-SAFE
  * generator.
  *
  * In MAD, we used openfile and openstream, but unifying doesn't change
  * the performance.
  *
  * Estimating the remaining time can be done externally, based on the
  * file description. This is equivalent to what is done currently,
  * except in WAV.
  *
  * The WAV decoder doesn't fit the approx duration computation.
  * The MIDI decoder doesn't use a buffer. TODO look at this carefully. *)

open Dtools

let log = Log.make ["decoder"]

(** A local file is simply identified by its filename. *)
type file = string

(** A stream is identified by a MIME type. *)
type stream = string

type 'a decoder = Decoder of ('a -> unit)

type input = int -> string * int

(** A stream decoder does not "own" any file descriptor,
  * and is generally assumed to not allocate resources (in the sense
  * of things that should be explicitly managed, not just garbage collected).
  * Hence it does not need a close function. *)
type stream_decoder = input -> Generator.From_audio_video_plus.t decoder

(** A decoder is a filling function and a closing function,
  * called at least when filling fails, i.e. the frame is partial.
  * The closing function can be called earlier e.g. if the user skips.
  * In most cases, file decoders are wrapped stream decoders. *)
type file_decoder = {
  fill : Frame.t -> int ; (* Return remaining ticks. *)
  close : unit -> unit ;
}

(** Plugins might define various decoders. In order to be accessed,
  * they should also register methods for choosing decoders. *)

(** For a given file, once a decoder is chosen it can be used several
  * times. This is at least useful to separate the actual opening of
  * the file from checking that it is a valid media file. *)
let file_decoders :
      (metadata:Frame.metadata -> file -> Frame.content_kind ->
         (unit -> file_decoder) option)
      Plug.plug =
  Plug.create
    ~doc:"File decoding methods." ~insensitive:true "file decoding"

let stream_decoders :
      (stream -> Frame.content_kind -> stream_decoder option) Plug.plug =
  Plug.create
    ~doc:"Stream decoding methods." ~insensitive:true "stream decoding"

let conf_http_source =
  Dtools.Conf.void ~p:(Configure.conf#plug "stream_decoding")
    "Stream decoding settings"
let conf_mime_types =
  Dtools.Conf.void ~p:(conf_http_source#plug "mime_types")
    "Mime-types used for guessing audio stream formats"
    ~comments:[
      "When a mime-type is available (e.g. with input.http), it can be used";
      "to guess which audio stream format is used.";
      "This section contains the listings used for that detection, which you";
      "might want to tweak if you encounter a new mime-type.";
      "If you feel that new mime-types should be permanently added, please";
      "contact the developpers."
    ]

let dummy =
  { fill = (fun b ->
      Frame.add_break b (Frame.position b) ;
      0) ;
    close = (fun _ -> ()) }

exception Exit of (string * (unit -> file_decoder))

(** Get a valid decoder creator for [filename]. *)
let get_file_decoder ~metadata filename kind : (unit -> file_decoder) option =
  try
    file_decoders#iter ~rev:true 
      (fun name decoder ->
         log#f 4 "Trying method %S for %S..." name filename ;
         match
           try decoder ~metadata filename kind with
             | e ->
                 log#f 4
                   "Decoder %S failed on %S: %s!"
                   name filename (Printexc.to_string e) ;
                 None
         with
           | Some f ->
               log#f 3 "Method %S accepted %S." name filename ;
               raise (Exit (name,f))
           | None -> ()) ;
    log#f 3
      "Unable to decode %S as %s!"
      filename (Frame.string_of_content_kind kind) ;
    None
  with
    | Exit (name,f) ->
        Some (fun () ->
                try f () with _ ->
                  log#f 2 "Decoder %S betrayed us on %S!" name filename ;
                  dummy)

exception Exit of stream_decoder

let get_stream_decoder mime kind =
  try
    stream_decoders#iter
      (fun name decoder ->
         log#f 4 "Trying method %S for %S..." name mime ;
         match try decoder mime kind with _ -> None with
           | Some f ->
               log#f 3 "Method %S accepted %S." name mime ;
               raise (Exit f)
           | None -> ()) ;
    log#f 3 "Unable to decode stream of type %S!" mime ;
    None
  with
    | Exit f -> Some f

(** {1 Helpers for defining decoders} *)

module Buffered(Generator:Generator.S) =
struct

  let file_decoder filename kind create_decoder gen =
    let frame_size = Lazy.force Frame.size in
    let prebuf =
      (* Amount of audio to decode in advance, in ticks.
       * It has to be more than a frame, but taking just one frame
       * is unsatisfying because it yields very low initial estimations
       * of the remaining time (which triggers early downloads,
       * transitions, etc). This is because the decoder will often
       * skip some hearders or metadata, giving the impression of
       * a poor compression rate.
       * We also guarantee that the remaining time will be precisely
       * given for the last [prebuf] seconds of a file. But in
       * practice it seems that we get pretty good estimations
       * way before that point... unless perhaps when there's a lot
       * of metadata (or ill-formed data) at the end of the file?
       * It seems that 0.5 seconds is enough. The more we put,
       * the higher will be the initial computation burst.
       * Putting a setting for that is probably too obscure to
       * be useful. *)
      Frame.master_of_seconds 0.5
    in
    let file_size = (Unix.stat filename).Unix.st_size in
    let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
    let input len =
      try
        let s = String.create len in
        let i = Unix.read fd s 0 len in
          s, i
      with _ -> "", 0
    in
    let Decoder decoder = create_decoder input in
    let out_ticks = ref 0 in
    let fill frame =
      begin try
        while Generator.length gen < prebuf do
          decoder gen
        done
      with e ->
        log#f 4 "Decoding %S failed: %s." filename (Printexc.to_string e)
      end ;

      let offset = Frame.position frame in
      let old_breaks = Frame.breaks frame in
      let c_end,content =
        Generator.fill gen frame ;
        Frame.content frame offset
      in
      let c_type = Frame.type_of_content content in
      let position = Frame.position frame in
        (* Check that we got only one chunk of data,
         * and that it has a correct type. *)
        if
          not (c_end = frame_size && Frame.type_has_kind c_type kind)
        then begin
          if c_end = frame_size then
            log#f 2
              "Decoder of %S produced %s, but %s was expected!"
              filename
              (Frame.string_of_content_type c_type)
              (Frame.string_of_content_kind kind)
          else
            log#f 2
              "Decoder of %S produced non-uniform data: \
               %s at %d, %s at %d! (End at %d)."
              filename
              (Frame.string_of_content_type c_type)
              offset
              (Frame.string_of_content_type
                 (Frame.type_of_content (snd (Frame.content frame c_end))))
              c_end
              position ;
          (* Pretend nothing happened, and end decoding.
           * We first restore a content layer with a valid type, so that
           * the code which reads that frame doesn't see the anomaly.
           * Then we reset breaks to indicate that there's no more data. *)
          let _ =
            Frame.content_of_type frame offset (Frame.type_of_kind kind)
          in
            Frame.set_breaks frame old_breaks ;
            Frame.add_break frame offset ;
            0
        end else
          let in_bytes = Unix.lseek fd 0 Unix.SEEK_CUR in
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
      { fill = fill ;
        close = fun () -> Unix.close fd }

end
