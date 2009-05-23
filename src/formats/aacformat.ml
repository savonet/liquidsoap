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

(** Decode and read metadatas of AAC files. *)

module Generator = Float_pcm.Generator

let log = Dtools.Log.make ["format";"aac"]

let decoder =
  let openfile file = 
    let dec = Faad.create () in
    let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
    try
      let abg = Generator.create () in
      let aacbuflen = 1024 in
      let aacbuf = String.create aacbuflen in
      let aacbufpos = ref aacbuflen in
      let pos = ref 0 in
      let fill_aacbuf () =
        String.blit aacbuf !aacbufpos aacbuf 0 (aacbuflen - !aacbufpos);
        let n = Unix.read fd aacbuf (aacbuflen - !aacbufpos) !aacbufpos in
        let n = !aacbufpos + n in
        aacbufpos := 0;
        pos := !pos + n;
        n
      in

      (* Dummy decoding in order to test format. *)
      let () =
        let offset, _, _ = Faad.init dec aacbuf 0 (Unix.read fd aacbuf 0 aacbuflen) in
        ignore (Unix.lseek fd offset Unix.SEEK_SET);
        let aacbuflen = fill_aacbuf () in
        if aacbuflen = 0 then raise End_of_file;
        if aacbuf.[0] <> '\255' then raise End_of_file;
        ignore (Faad.decode dec aacbuf 0 aacbuflen);
        ignore (Unix.lseek fd 0 Unix.SEEK_SET)
      in

      let offset, sample_freq, chans =
        Faad.init dec aacbuf 0 (Unix.read fd aacbuf 0 aacbuflen)
      in
      aacbufpos := aacbuflen;
      ignore (Unix.lseek fd offset Unix.SEEK_SET);
      (dec,fd,aacbuf,fill_aacbuf,aacbufpos,sample_freq,pos),abg
    with
      | e -> Unix.close fd; raise e
  in
  let close (dec,fd,_,_,_,_,_) =
    Faad.close dec;
    Unix.close fd
  in
  let decode (dec,_,aacbuf,fill_aacbuf,aacbufpos,sample_freq,pos) abg = 
   try
    let aacbuflen = fill_aacbuf () in
    if aacbuflen = 0 then raise End_of_file;
    if aacbuf.[0] <> '\255' then raise End_of_file;
    let pos, buf = Faad.decode dec aacbuf 0 aacbuflen in
    aacbufpos := pos;
    Generator.feed abg ~sample_freq buf
   with
     | Faad.Error n ->
                log#f 4 "Faad error: %s\n%!" (Faad.error_message n)
  in
  let position (_,_,_,_,_,_,pos) = !pos in
  let decoder =
   {
    File_decoder.Float.
     log = log;
     openfile = openfile;
     decode = decode;
     position = position;
     close = close
   }
  in
  File_decoder.Float.decode decoder

let log = Dtools.Log.make ["format";"aacmp4"]

let decoder_mp4 =
  let openfile file =
    let dec = Faad.create () in
    let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
    try
      let mp4 = Faad.Mp4.openfile_fd fd in
      let abg = Generator.create () in
      let track = Faad.Mp4.find_aac_track mp4 in
      let sample_freq, chans = Faad.Mp4.init mp4 dec track in
      let samples = Faad.Mp4.samples mp4 track in
      let sample = ref 0 in
      (dec,fd,track,mp4,sample,samples,sample_freq),abg
    with
      | e -> Unix.close fd; raise e
  in
  let close (dec,fd,_,_,_,_,_) =
    Faad.close dec;
    Unix.close fd
  in
  let decode (dec,fd,track,mp4,sample,samples,sample_freq) abg = 
   try
    if !sample >= samples then raise End_of_file;
    Generator.feed abg ~sample_freq (Faad.Mp4.decode mp4 track !sample dec);
    incr sample
   with
     | Faad.Error n ->
         log#f 4 "Faad error: %s\n%!" (Faad.error_message n)
  in
  let position (_,fd,_,_,_,_,_) = Unix.lseek fd 0 Unix.SEEK_CUR in
  let decoder =
   {
    File_decoder.Float.
     log = log;
     openfile = openfile;
     decode = decode;
     position = position;
     close = close
   }
  in
  File_decoder.Float.decode decoder

let () =
  Decoder.formats#register "AAC"
    (fun name -> try Some (decoder name) with _ -> None);
  Decoder.formats#register "AACMP4"
    (fun name -> try Some (decoder_mp4 name) with _ -> None)

let get_tags ~format file =
  if format <> "AACMP4" then raise Not_found;
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
    try
      let mp4 = Faad.Mp4.openfile_fd fd in
      let m = Array.to_list (Faad.Mp4.metadata mp4) in
        Unix.close fd;
        m
    with
      | e -> Unix.close fd; raise e

let () = Request.mresolvers#register "AACMP4" get_tags
