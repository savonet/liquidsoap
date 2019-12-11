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
  * In MP3, we used openfile and openstream, but unifying doesn't change
  * the performance.
  *
  * Estimating the remaining time can be done externally, based on the
  * file description. This is equivalent to what is done currently,
  * except in WAV.
  *
  * The WAV decoder doesn't fit the approx duration computation. (Toots: is that true?)
  * The MIDI decoder doesn't use a buffer. TODO look at this carefully. *)

let log = Log.make ["decoder"]

(** A local file is simply identified by its filename. *)
type file = string

(** A stream is identified by a MIME type. *)
type stream = string

type 'a decoder = {
  decode: 'a -> unit;
  (* [seek x]: Skip [x] master ticks.
   * Returns the number of ticks atcually skiped. *)
  seek: int -> int;
}

type input = {
  read: bytes -> int -> int -> int;
  (* Seek to an absolute position in bytes. 
   * Returns the current position after seeking. *)
  lseek: (int -> int) option;
  tell: (unit -> int) option;
  length: (unit -> int) option;
}

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
  fill: Frame.t -> int;
  (* Return remaining ticks. *)
  fseek: int -> int;
  (* There is a record name clash here.. *)
  close: unit -> unit;
}

(** Plugins might define various decoders. In order to be accessed,
  * they should also register methods for choosing decoders. *)

let conf_decoder =
  Dtools.Conf.void ~p:(Configure.conf#plug "decoder") "Decoder settings"

let conf_file_decoders =
  Dtools.Conf.list
    ~p:(conf_decoder#plug "file_decoders")
    ~d:[] "Decoders and order used to decode files."

let conf_image_file_decoders =
  Dtools.Conf.list
    ~p:(conf_decoder#plug "image_file_decoders")
    ~d:[] "Decoders and order used to decode image files."

let conf_stream_decoders =
  Dtools.Conf.list
    ~p:(conf_decoder#plug "stream_decoders")
    ~d:[] "Decoders and order used to decode streams."

let f c v =
  match c#get_d with
    | None ->
        c#set_d (Some [v])
    | Some d ->
        c#set_d (Some (d @ [v]))

let get_decoders conf decoders =
  let f cur name =
    match decoders#get name with
      | Some p ->
          (name, p) :: cur
      | None ->
          log#severe "Cannot find decoder %s" name ;
          cur
  in
  List.fold_left f [] (List.rev conf#get)

(** For a given file, once a decoder is chosen it can be used several
  * times. This is at least useful to separate the actual opening of
  * the file from checking that it is a valid media file. *)
let file_decoders :
    (metadata:Frame.metadata ->
    file ->
    Frame.content_kind ->
    (unit -> file_decoder) option)
    Plug.plug =
  Plug.create
    ~register_hook:(fun (name, _) -> f conf_file_decoders name)
    ~doc:"File decoding methods." ~insensitive:true "file decoding"

let image_file_decoders : (file -> Video.Image.t option) Plug.plug =
  Plug.create
    ~register_hook:(fun (name, _) -> f conf_image_file_decoders name)
    ~doc:"Image file decoding methods." ~insensitive:true "image file decoding"

let stream_decoders :
    (stream -> Frame.content_kind -> stream_decoder option) Plug.plug =
  Plug.create
    ~register_hook:(fun (name, _) -> f conf_stream_decoders name)
    ~doc:"Stream decoding methods." ~insensitive:true "stream decoding"

let conf_debug =
  Dtools.Conf.bool
    ~p:(conf_decoder#plug "debug")
    ~d:false "Maximum debugging information (dev only)"
    ~comments:
      [ "WARNING: Do not enable unless a developer instructed you to do so!";
        "The debugging mode makes it easier to understand why decoding fails,";
        "but as a side effect it will crash liquidsoap at the end of every";
        "track." ]

let conf_mime_types =
  Dtools.Conf.void
    ~p:(conf_decoder#plug "mime_types")
    "Mime-types used for guessing audio formats"
    ~comments:
      [ "When a mime-type is available (e.g. with input.http), it can be used";
        "to guess which audio stream format is used.";
        "This section contains the listings used for that detection, which you";
        "might want to tweak if you encounter a new mime-type.";
        "If you feel that new mime-types should be permanently added, please";
        "contact the developpers." ]

let conf_file_extensions =
  Dtools.Conf.void
    ~p:(conf_decoder#plug "file_extensions")
    "File extensions used for guessing audio formats"

let test_file ?(log = log) ~mimes ~extensions fname =
  if not (Sys.file_exists fname) then (
    log#info "File %S does not exist!" fname ;
    false )
  else (
    let ext_ok =
      try List.mem (Utils.get_ext fname) extensions with _ -> false
    in
    let mime_ok, mime =
      match Configure.file_mime with
        (* If no mime detection is available
         * set the same result as file extension.. *)
        | None ->
            (ext_ok, None)
        | Some mime_type ->
            let mime = mime_type fname in
            (List.mem mime mimes, Some mime)
    in
    if ext_ok || mime_ok then true
    else (
      if (not mime_ok) && mime <> None then
        log#info "Invalid MIME type for %S: %s!" fname (Utils.get_some mime) ;
      if not ext_ok then log#info "Invalid file extension for %S!" fname ;
      false ) )

let dummy =
  {
    fill=
      (fun b ->
        Frame.add_break b (Frame.position b) ;
        0);
    fseek= (fun _ -> 0);
    close= (fun _ -> ());
  }

exception Exit of (string * (unit -> file_decoder))

(** Get a valid decoder creator for [filename]. *)
let get_file_decoder ~metadata filename kind =
  try
    List.iter
      (fun (name, decoder) ->
        log#info "Trying method %S for %S..." name filename ;
        match
          try decoder ~metadata filename kind
          with e ->
            log#info "Decoder %S failed on %S: %s!" name filename
              (Printexc.to_string e) ;
            None
        with
          | Some f ->
              log#important "Method %S accepted %S." name filename ;
              raise (Exit (name, f))
          | None ->
              ())
      (get_decoders conf_file_decoders file_decoders) ;
    log#important "Unable to decode %S as %s!" filename
      (Frame.string_of_content_kind kind) ;
    None
  with Exit (name, f) ->
    Some
      ( name,
        fun () ->
          try f ()
          with exn ->
            log#severe "Decoder %S betrayed us on %S! Error: %s\n%s" name
              filename (Printexc.to_string exn)
              (Printexc.get_backtrace ()) ;
            dummy )

(** Get a valid image decoder creator for [filename]. *)
let get_image_file_decoder filename =
  let ans = ref None in
  try
    List.iter
      (fun (name, decoder) ->
        log#info "Trying method %S for %S..." name filename ;
        match
          try decoder filename
          with e ->
            log#info "Decoder %S failed on %S: %s!" name filename
              (Printexc.to_string e) ;
            None
        with
          | Some img ->
              log#important "Method %S accepted %S." name filename ;
              ans := Some img ;
              raise Stdlib.Exit
          | None ->
              ())
      (get_decoders conf_image_file_decoders image_file_decoders) ;
    log#important "Unable to decode %S!" filename ;
    !ans
  with Stdlib.Exit -> !ans

exception Exit_decoder of stream_decoder

let get_stream_decoder mime kind =
  try
    List.iter
      (fun (name, decoder) ->
        log#info "Trying method %S for %S..." name mime ;
        match try decoder mime kind with _ -> None with
          | Some f ->
              log#important "Method %S accepted %S." name mime ;
              raise (Exit_decoder f)
          | None ->
              ())
      (get_decoders conf_stream_decoders stream_decoders) ;
    log#important "Unable to decode stream of type %S!" mime ;
    None
  with Exit_decoder f -> Some f

(** {1 Helpers for defining decoders} *)

module Buffered (Generator : Generator.S) = struct
  let make_file_decoder ~filename ~close ~kind ~remaining decoder gen =
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
    let decoding_done = ref false in
    let fill frame =
      (* We want to avoid trying to decode when
       * it is no longer possible. Hence, this loop
       * will be executed iff there was no exception
       * raised before. If a decoder wants to recover
       * on an exception, it should catch it and deal
       * with it on its own. 
       * Hence, the current policy for decoding is:
       * decoding stops at the first exception raised. *)
      if not !decoding_done then (
        try
          while Generator.length gen < prebuf do
            decoder.decode gen
          done
        with e ->
          log#info "Decoding %S ended: %s." filename (Printexc.to_string e) ;
          log#debug "%s" (Printexc.get_backtrace ()) ;
          decoding_done := true ;
          if conf_debug#get then raise e ) ;
      let offset = Frame.position frame in
      let old_breaks = Frame.breaks frame in
      let c_end, content =
        Generator.fill gen frame ; Frame.content frame offset
      in
      let c_type = Frame.type_of_content content in
      let position = Frame.position frame in
      (* Check that we got only one chunk of data,
       * and that it has a correct type. *)
      if not (c_end = frame_size && Frame.type_has_kind c_type kind) then (
        if c_end = frame_size then
          log#severe "Decoder of %S produced %s, but %s was expected!" filename
            (Frame.string_of_content_type c_type)
            (Frame.string_of_content_kind kind)
        else
          log#severe
            "Decoder of %S produced non-uniform data: %s at %d, %s at %d! \
             (End at %d)."
            filename
            (Frame.string_of_content_type c_type)
            offset
            (Frame.string_of_content_type
               (Frame.type_of_content (snd (Frame.content frame c_end))))
            c_end position ;
        (* Pretend nothing happened, and end decoding.
         * We first restore a content layer with a valid type, so that
         * the code which reads that frame doesn't see the anomaly.
         * Then we reset breaks to indicate that there's no more data. *)
        let _ = Frame.content_of_type frame offset (Frame.type_of_kind kind) in
        Frame.set_breaks frame old_breaks ;
        Frame.add_break frame offset ;
        0 )
      else (
        try if not !decoding_done then remaining frame offset else 0
        with e ->
          log#info "Error while getting decoder's remaining time: %s"
            (Printexc.to_string e) ;
          decoding_done := true ;
          0 )
    in
    let fseek len =
      let gen_len = Generator.length gen in
      if len < 0 || len > gen_len then (
        Generator.clear gen ;
        gen_len + decoder.seek (len - gen_len) )
      else (
        (* Seek within the pre-buffered data if possible *)
        Generator.remove gen len ; len )
    in
    {fill; fseek; close}

  let file_decoder filename kind create_decoder gen =
    let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
    let file_size = (Unix.stat filename).Unix.st_size in
    let proc_bytes = ref 0 in
    let read buf ofs len =
      try
        let i = Unix.read fd buf ofs len in
        proc_bytes := !proc_bytes + i ;
        i
      with _ -> 0
    in
    let tell () = Unix.lseek fd 0 Unix.SEEK_CUR in
    let length () = (Unix.fstat fd).Unix.st_size in
    let lseek len = Unix.lseek fd len Unix.SEEK_SET in
    let input =
      {read; tell= Some tell; length= Some length; lseek= Some lseek}
    in
    let decoder = create_decoder input in
    let out_ticks = ref 0 in
    let remaining frame offset =
      let in_bytes = tell () in
      let gen_len = Generator.length gen in
      out_ticks := !out_ticks + Frame.position frame - offset ;
      (* Compute an estimated number of remaining ticks. *)
      if !proc_bytes = 0 then -1
      else (
        let compression = float (!out_ticks + gen_len) /. float !proc_bytes in
        let remaining_ticks =
          float gen_len +. (float (file_size - in_bytes) *. compression)
        in
        int_of_float remaining_ticks )
    in
    let close () = Unix.close fd in
    make_file_decoder ~filename ~close ~kind ~remaining decoder gen
end
