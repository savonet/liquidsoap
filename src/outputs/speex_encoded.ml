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

open Source

exception Internal

let speex_proto = [
  "samplerate",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some ("Output sample rate. Use liquidsoap's default if <= 0.");

  "bitrate",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some "Target bitrate (in kbps). Not used if <= 0.";

  "quality",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some "Target quality (0 to 10). Not used if <= 0.";

  "mode",
  Lang.string_t,
  Some (Lang.string "narrowband"),
  Some "Encoding mode, one of \"narrowband\", \"wideband\" \
        or \"ultra-wideband\".";

  "stereo",
  Lang.bool_t,
  Some (Lang.bool false),
  None;

  "vbr",
  Lang.bool_t,
  Some (Lang.bool true),
  Some "Encode in vbr mode.";

  "frames_per_packet",
  Lang.int_t,
  Some (Lang.int 1),
  Some "Number of frame per Ogg packet (1 to 10).";

  "complexity",
  Lang.int_t,
  Some (Lang.int 3),
  Some "Encoding complexity (0-10).";

  "abr",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some "Set average bitrate. Not used if <= 0.";
]

class virtual base freq stereo mode bitrate vbr fpp complexity abr quality =
  let samples_per_second = float (Fmt.samples_per_second()) in
  let freq = 
    if freq > 0 then
      freq
    else
      int_of_float samples_per_second
  in
  let stereo = 
    if Fmt.channels () = 1 then
      false
    else
      stereo
  in
object (self)
  val tmp =
    if stereo then
      [||]
    else
      Float_pcm.create_buffer 1 (Fmt.samples_per_frame())

  val virtual mutable encoder : Ogg_encoder.t option

  val mutable stream_id = None

  method new_encoder stereo meta =
    assert(stream_id = None);
    let enc = 
      Speex_format.create ~frames_per_packet:fpp ~mode ~vbr ~quality
                          ~stereo ~bitrate ~rate:freq ~abr ~complexity 
                          ~meta ()
    in
    let ogg_enc = Utils.get_some encoder in
    stream_id <- Some (Ogg_encoder.register_track ogg_enc enc)

  method reset_encoder m =
    let rec get l l' =
      match l with
        | k :: r ->
          begin 
            try
              get r ((k,Hashtbl.find m k) :: l')
            with _ -> get r l'
          end
        | [] -> l'
    in
    let title =
      try
        Hashtbl.find m "title"
      with
        | _ -> 
        begin
          try
            let s = Hashtbl.find m "uri" in
            let title = Filename.basename s in
                (try
                   String.sub title 0 (String.rindex title '.')
                 with
                   | Not_found -> title)
          with
            | _ -> "Unknown"
        end
    in
    let l' = ["title",title] in
    let tags = get ["artist";"genre";"date";
                    "album";"tracknumber";"comment"]
                   l'
    in
    let enc = Utils.get_some encoder in
    let id = Utils.get_some stream_id in
    Ogg_encoder.end_of_track enc id;
    stream_id <- None;
    let flushed = Ogg_encoder.flush enc in
    self#new_encoder stereo tags;
    flushed 

  method encode frame start len =
    let b = AFrame.get_float_pcm frame in
    let start = Fmt.samples_of_ticks start in
    let len = Fmt.samples_of_ticks len in
    let b =
      if stereo then b else begin
        for i = start to start+len do
	  let n = Fmt.channels () in
	  let f i = 
	    Array.fold_left (fun x y -> x +. y.(i)) 0. b
	  in
          tmp.(0).(i) <- f i /. (float_of_int n)
        done ;
        tmp
      end
    in
    let (buf,start,len) =
      if float freq <> samples_per_second then
        let b = 
           Float_pcm.resample
            (float freq /. samples_per_second)
            b start len
        in
        b,0,Array.length b.(0)
      else
        b,start,len
    in
    let data =
     Ogg_encoder.Audio_data
      {
       Ogg_encoder.
        data   = buf;
        offset = start;
        length = len
      }
    in
    let enc = Utils.get_some encoder in
    let id = Utils.get_some stream_id in
    Ogg_encoder.encode enc id data;
    Ogg_encoder.get_data enc
end

(** Output in an Ogg/speex file. *)
class to_file
  filename ~append ~perm ~dir_perm ~vbr ~fpp ~quality
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~mode ~bitrate ~complexity ~abr freq stereo source autostart =
object (self)
  inherit
    [Ogg_encoder.t] Output.encoded
      ~name:filename ~kind:"output.file" ~autostart source
  inherit File_output.to_file
            ~reload_delay ~reload_predicate ~reload_on_metadata
            ~append ~perm ~dir_perm filename as to_file
  inherit Ogg_output.base as ogg
  inherit base freq stereo mode bitrate vbr fpp complexity abr quality as base

  method reset_encoder m =
    to_file#on_reset_encoder ;
    to_file#set_metadata (Hashtbl.find (Hashtbl.copy m)) ;
    base#reset_encoder m

  method output_start = 
    ogg#output_start;
    self#new_encoder stereo [] ;
    to_file#file_output_start 

  method output_stop =
    let f = ogg#end_of_stream in
    ogg#output_stop;
    to_file#send f ;
    to_file#file_output_stop 

end

let () =
  Lang.add_operator "output.file.speex"
    (speex_proto @ File_output.proto @ [
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg speex file.")
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
       let bitrate = (e Lang.to_int "bitrate") in
       let bitrate = 
         if bitrate > 0 then
           bitrate * 1000
         else
           bitrate
       in
       let vbr = e Lang.to_bool "vbr" in
       let fpp = e Lang.to_int "frames_per_packet" in
       let freq = e Lang.to_int "samplerate" in
       let quality = e Lang.to_int "quality" in
       let abr = (e Lang.to_int "abr") * 1000 in
       let complexity = e Lang.to_int "complexity" in
       let mode = 
         match e Lang.to_string "mode" with
           | "narrowband" -> Speex.Narrowband
           | "wideband" -> Speex.Wideband
           | "ultra-wideband" -> Speex.Ultra_wideband
           | _ -> failwith "Unknown speex mode"
       in
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
       let source = Lang.assoc "" 2 p in
         ((new to_file
             name ~append ~perm ~dir_perm ~quality
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~mode ~bitrate ~vbr ~fpp ~complexity ~abr
             freq stereo source autostart):>source))

