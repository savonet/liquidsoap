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

(** Decode and read ogg files. *)

let log = Dtools.Log.make ["format";"ogg"]

module Generator = Generator.From_audio_video

exception Channels of int

let converter () =
  let current_format = ref None in
  (fun format ->
    let format =
      match format with
        | Ogg_demuxer.Yuvj_422 -> Video_converter.Yuvj_422
        | Ogg_demuxer.Yuvj_420 -> Video_converter.Yuvj_420
        | Ogg_demuxer.Yuvj_444 -> Video_converter.Yuvj_444
    in
    match !current_format with
      | Some x when fst(x) = format -> snd(x)
      | _ ->
        let converter =
          Video_converter.find_converter
             (Video_converter.YUV format)
             (Video_converter.RGB Video_converter.Rgba_32)
        in
        current_format := Some (format,converter) ;
        converter)

(** Convert a video frame to RGB *)
let video_convert () =
  let converter = converter () in
  (fun buf ->
    let converter = converter buf.Ogg_demuxer.format in
    let width = Lazy.force Frame.video_width in
    let height = Lazy.force Frame.video_height in
    let rgb = RGB.create width height in
    let frame = Video_converter.frame_of_internal_rgb rgb in
    converter
      (Video_converter.frame_of_internal_yuv
         buf.Ogg_demuxer.width buf.Ogg_demuxer.height
        ((buf.Ogg_demuxer.y, buf.Ogg_demuxer.y_stride),
            (buf.Ogg_demuxer.u,
             buf.Ogg_demuxer.v,
             buf.Ogg_demuxer.uv_stride)))
      frame;
    rgb)

(** Stupid nearest neighbour resampling.
  * For meaningful results, one should first partially apply the freq params,
  * and re-use the resulting functions on consecutive chunks of a single
  * input stream. *)
let resample ~in_freq ~out_freq =
  (* We have something like this:
   *
   * i i i i i i i i i i i i i i i i i i i ...
   * o     o       o     o       o     o   ...
   *
   * (1) We ensure that out_len/out_freq = in_len/in_freq asymptotically.
   *     For doing so, we must keep track of the full input length,
   *     modulo in_freq.
   * (2) We do the simplest possible thing to choose which i becomes
   *     which o: nearest neighbour in the currently available buffer.
   *     This is not as good as nearest neighbour in the real stream.
   *
   * Turns out the same code codes for when out_freq>in_freq too. *)
  let in_pos = ref 0 in
    fun input off len ->
      let new_in_pos = !in_pos+len in
      let already_out_len = !in_pos * out_freq / in_freq in
      let needed_out_len = new_in_pos * out_freq / in_freq in
      let out_len = needed_out_len - already_out_len in
        in_pos := new_in_pos mod in_freq ;
        Array.init
          out_len
          (fun i -> input.(off + i * in_freq / out_freq))

let video_resample () =
  let in_out = ref None in
  let resampler = ref None in
    fun ~in_freq ~out_freq buf off len ->
      if !in_out = Some (in_freq,out_freq) then
        (Utils.get_some !resampler) buf off len
      else begin
        in_out := Some (in_freq,out_freq) ;
        resampler := Some (resample ~in_freq ~out_freq) ;
        (Utils.get_some !resampler) buf off len
      end

type state = {
  decoder : Ogg_demuxer.t ;
  fd : Unix.file_descr ;
  video_convert : Ogg_demuxer.video -> RGB.t ;
  audio_resample : ?audio_src_rate:float -> Float_pcm.pcm -> Float_pcm.pcm*int ;
  video_resample : in_freq:int -> out_freq:int ->
                   RGB.t array -> int -> int -> RGB.t array
}

let decoder =
  {
   File_decoder.
    log = log;
    openfile =
      (fun file ->
        let sync,fd = Ogg.Sync.create_from_file file in
        begin
         try
           {
             decoder = Ogg_demuxer.init sync ;
             fd = fd ;
             video_convert = video_convert () ;
             audio_resample = Rutils.create_audio () ;
             video_resample = video_resample ()
           }
         with
           | e -> (try Unix.close fd with _ -> ()); raise e
        end);
    get_kind =
      (fun {decoder=decoder} ->
        let feed ((buf,_),_) =
          raise (Channels (Array.length buf))
        in
        let audio =
          try
            Ogg_demuxer.decode_audio_rec decoder feed;
            raise Not_found
          with
            | Channels x -> x
            | Not_found  -> 0
        in
        let feed (buf,_) =
          raise (Channels 1)
        in
        let video =
          try
            Ogg_demuxer.decode_video_rec decoder feed;
            raise Not_found
          with
            | Channels x -> x
            | Not_found  -> 0
        in
        { Frame.
            audio = Frame.mul_of_int audio ;
            video = Frame.mul_of_int video ;
            midi  = Frame.mul_of_int 0 });
    decode =
      (fun s buffer ->
          if Ogg_demuxer.eos s.decoder then
            raise Ogg_demuxer.End_of_stream;
          let feed ((buf,sample_freq),_) =
            let audio_src_rate = float sample_freq in
            let content,length =
              s.audio_resample ~audio_src_rate buf
            in
            Generator.put_audio buffer content 0 length
          in
          let got_audio =
           try
            Ogg_demuxer.decode_audio s.decoder feed ;
            true
           with
             | Not_found
             | Ogg.Not_enough_data -> false
          in
          let feed (buf,_) =
            let in_freq = int_of_float buf.Ogg_demuxer.fps in
            let out_freq = Lazy.force Frame.video_rate in
            let rgb = s.video_convert buf in
            let stream = s.video_resample ~in_freq ~out_freq [|rgb|] 0 1 in
              Generator.put_video buffer [|stream|] 0 (Array.length stream)
          in
          (* Try to decode video. *)
          let got_video =
            try
              Ogg_demuxer.decode_video s.decoder feed ;
              true
            with
              | Not_found
              | Ogg.Not_enough_data -> false
          in
          if not got_audio && not got_video then
            Ogg_demuxer.feed s.decoder);
    position = (fun s -> Unix.lseek s.fd 0 Unix.SEEK_CUR);
    close = (fun s ->
               try
                 Unix.close s.fd
               with _ -> ())
  }

let () = Decoder.formats#register "OGG" (File_decoder.decode decoder)

exception Metadata of (string*string) list

let get_tags ~format file =
  (* Fail if file is not decoded using the OGG demuxer. *)
  if format <> "OGG" then
    raise Not_found;
  let sync,fd = Ogg.Sync.create_from_file file in
  let close () =
    try
      Unix.close fd
    with
      | _ -> ()
  in
  try
    let decoder = Ogg_demuxer.init sync in
    let feed (_,m) =
      let m =
        match m with
          | Some m ->
             Hashtbl.fold
              (fun x -> fun y -> fun z -> (x,y)::z)
              m []
          | None -> []
      in
      raise (Metadata m)
    in
    let m =
      try
        if Ogg_demuxer.has_track Ogg_demuxer.Audio_track decoder then
          Ogg_demuxer.decode_audio_rec decoder feed;
        []
      with
        | Metadata m -> m
    in
    let m =
      try
        if Ogg_demuxer.has_track Ogg_demuxer.Video_track decoder then
          Ogg_demuxer.decode_video_rec decoder feed;
        m
      with
        | Metadata m' -> m@m'
    in
    close ();
    m
  with
    | _ -> close (); []

let () = Request.mresolvers#register "OGG" get_tags
