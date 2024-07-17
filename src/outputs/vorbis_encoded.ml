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

open Source
open Vorbis

(** Create a vorbis encoder which output has the given format.
  * Its input should be signed 16 bit little-endian, with the same number of
  * channels and samplerate as the output. *)
let get_ogg_enc_params ?stereo ?freq ~quality bitrate =
  {
    Vorbis.enc_min_bitrate = None;
    Vorbis.enc_max_bitrate = None;
    Vorbis.enc_quality = quality ;
    Vorbis.enc_managed = false;
    Vorbis.enc_bitrate = if bitrate>0 then Some bitrate else None ;
    Vorbis.enc_channels = if stereo = Some false then 1 else 2 ;
    Vorbis.enc_sample_freq = if freq = None then Some 44100 else freq ;

    (* Vorbis' internal conversions are not needed anymore.
     * They should be removed or fixed anyway. *)
    Vorbis.enc_in_channels = if stereo = Some false then 1 else 2 ;
    Vorbis.enc_in_sample_freq = (match freq with None -> 44100 | Some f -> f) ;
    Vorbis.enc_in_sample_size = 16;
    Vorbis.enc_in_big_endian = false;
  }

let reset encoder m =
  let get h k =
    try
      Some (Hashtbl.find h k)
    with _ -> None
  in
  let getd h k d =
    try
      Some (Hashtbl.find h k)
    with _ -> Some d
  in
  let def_title =
    match get m "uri" with
      | Some s -> let title = Filename.basename s in
          ( try
              String.sub title 0 (String.rindex title '.')
            with
              | Not_found -> title )
      | None -> "Unknown"
  in
    Vorbis.encoder_reset_opt
      (getd m "title" def_title)
      (get m "artist")
      (get m "genre")
      (get m "date")
      (get m "album")
      (get m "tracknum")
      (get m "comment")
      encoder

(** Output in an Ogg/vorbis file. *)

class virtual base freq stereo =
  let samples_per_second = float (Fmt.samples_per_second()) in
object (self)
  val tmp = [| Array.create (Fmt.samples_per_frame()) 0. |]
  method reset_encoder encoder m = reset encoder m
  method encode e b start len =
    let b =
      if stereo then b else begin
        for i = start to start+len-1 do
          tmp.(0).(i) <- (b.(0).(i) +. b.(1).(i)) /. 2.
        done ;
        tmp
      end
    in
    let buf =
      Float_pcm.resample
        (float freq /. samples_per_second)
        b start len
    in
      Vorbis.encode_buffer_float_part e buf 0 (Array.length buf.(0))
end

class to_file
  filename ~append ~perm ~dir_perm
  ~reload_delay ~reload_predicate
  ~quality bitrate freq stereo source autostart =
object (self)
  inherit
    [Vorbis.encoder] Output.encoded
      ~name:filename ~kind:"output.file.vorbis" ~autostart source

  inherit base freq stereo

  val mutable fd = None
  val mutable open_date = 0.

  (** (re-)open the output file *)
  method output_start =
    assert (fd = None) ;
    let enc,first_header =
      Vorbis.create_encoder
        (get_ogg_enc_params ~stereo ~freq ~quality bitrate)
        ~title:"Liquidsoap" ~artist:"The Savonet Team"
    in
    let mode =
      Open_wronly::Open_creat::
      (if append then [Open_append] else [Open_trunc])
    in
    let filename = Utils.strftime filename in
    let filename = Utils.home_unrelate filename in
    let chan =
      Utils.mkdir ~perm:dir_perm (Filename.dirname filename) ;
      open_out_gen mode perm filename
    in
      set_binary_mode_out chan true ;
      open_date <- Unix.gettimeofday () ;
      fd <- Some chan ;
      encoder <- Some enc ;
      self#send first_header

  method output_stop =
    match fd with
      | None -> assert false
      | Some v -> close_out v ; fd <- None

  method output_reset = ()

  method send b =
    if Unix.gettimeofday () > reload_delay +. open_date then
      if Lang.to_bool (Lang.apply reload_predicate []) then begin
        self#log 3 "Re-opening output file..." ;
        self#output_stop ;
        self#output_start
      end ;
    output_string (Utils.get_some fd) b

end

let () =
  Lang.add_operator "output.file.vorbis"
    [ "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ;

      "quality",
      Lang.float_t,
      Some (Lang.float 0.5),
      None;

      "bitrate",
      Lang.int_t,
      Some (Lang.int (-1)),
      None;

      "samplerate",
      Lang.int_t,
      Some (Lang.int 44100),
      None;

      "stereo",
      Lang.bool_t,
      Some (Lang.bool true),
      None;

      "",
      Lang.string_t,
      None,
      Some ("Filename where to output the OGG stream. "^
            "Some strftime conversion specifiers are available: %SMHdmY.") ;

      "append",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Do not truncate but append in the file if it exists." ;

      "perm",
      Lang.int_t,
      Some (Lang.int 0o666),
      Some "Permission of the file if it has to be created, up to umask." ;

      "dir_perm",
      Lang.int_t,
      Some (Lang.int 0o777),
      Some ("Permission of the directories if some have to be created, "^
            "up to umask.") ;

      "reload_delay", Lang.float_t, Some (Lang.float 120.),
      Some "Prevent re-opening of the file within that delay, in seconds." ;

      "when", Lang.fun_t [] Lang.bool_t,
      Some (Lang.val_fun [] (Lang.bool false)),
      Some "When should the output file be re-opened." ;

      "", Lang.source_t, None, None ]
    ~category:Lang.Output
    ~descr:"Output the source's stream as an ogg vorbis file."
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
       let bitrate = e Lang.to_int "bitrate" in
       let quality = e Lang.to_float "quality" in
       let freq = e Lang.to_int "samplerate" in
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "when" p in
       let reload_delay = Lang.to_float (List.assoc "reload_delay" p) in
       let source = Lang.assoc "" 2 p in
         ((new to_file
             name ~append ~perm ~dir_perm
             ~reload_delay ~reload_predicate
             ~quality bitrate freq stereo source autostart):>source))

(** Send Vorbis through a shout connection *)

let no_mount = "Use [name].ogg"
let no_name = "Use [mount]"

let proto =
  Icecast2.proto @
  [ "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;

    "bitrate", Lang.int_t, Some (Lang.int (-1)), None;
    "quality", Lang.float_t, Some (Lang.float 0.5), None;
    "samplerate", Lang.int_t, Some (Lang.int 44100), None;
    "stereo", Lang.bool_t, Some (Lang.bool true), None;
    "", Lang.source_t, None, None ]

class to_shout p =

  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let name = s  "name" in
  let mount = s "mount" in
  let name =
    if name = no_name then
      if mount = no_mount then
        raise (Lang.Invalid_value
                 ((List.assoc "mount" p),
                  "Either name or mount must be defined."))
      else
        mount
    else
      name
  in
  let mount =
    if mount = no_mount then name ^ ".ogg" else mount
  in

  let stereo = e Lang.to_bool "stereo" in
  let bitrate = e Lang.to_int "bitrate" in
  let quality = e Lang.to_float "quality" in
  let freq = e Lang.to_int "samplerate" in

  let source = List.assoc "" p in

object (self)
  inherit [Vorbis.encoder] Icecast2.output ~mount ~name ~source p as super
  inherit base freq stereo

  method output_start =
    super#output_start ;
    let enc,first_header =
      log 3 "Setting up an Ogg/Vorbis encoder..." ;
      Vorbis.create_encoder
        (get_ogg_enc_params ~stereo ~freq ~quality bitrate)
        ~title:"Liquidsoap" ~artist:"The Savonet Team"
    in
      encoder <- Some enc ;

      log 3 "Sending the first header ..." ;
      self#send first_header
end

let () =
  Lang.add_operator "output.icecast.vorbis" proto ~category:Lang.Output
    ~descr:"Send a Vorbis stream to an icecast-compatible server."
    (fun p -> ((new to_shout p):>Source.source))
