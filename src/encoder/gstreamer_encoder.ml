(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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

(** GStreamer encoder *)

open Encoder.GStreamer

module GU = Gstreamer_utils
module Img = Image.RGBA32

type gst =
  {
    bin : Gstreamer.Pipeline.t;
    audio_src : Gstreamer.App_src.t;
    video_src : Gstreamer.App_src.t;
    sink : Gstreamer.App_sink.t;
  }

let encoder id ext =
  GU.init ();
  let channels =
    Encoder.GStreamer.audio_channels ext 
  in
  let mutex = Mutex.create () in
  let samples = ref 0 in
  let decr_samples =
    Tutils.mutexify mutex (fun () ->
      decr samples)
  in
  let incr_samples =
    Tutils.mutexify mutex (fun () ->
      incr samples)
  in
  let on_sample () =
    incr_samples();
  in

  let gst =
    let audio_pipeline =
      Stdlib.maybe (fun pipeline ->
        Printf.sprintf "%s ! queue ! %s ! %s ! muxer."
          (GU.Pipeline.audio_src ~channels ~block:false "audio_src")
          (GU.Pipeline.convert_audio ())
          pipeline) ext.audio_pipeline
    in
    let video_pipeline =
      Stdlib.maybe (fun pipeline ->
        Printf.sprintf "%s ! queue ! %s ! %s ! muxer."
          (GU.Pipeline.video_src ~block:true "video_src")
          (GU.Pipeline.convert_video ())
          pipeline) ext.video_pipeline
    in
    let pipeline =
      Printf.sprintf "%s %s avimux name=muxer ! appsink name=sink sync=false emit-signals=true"
        (Stdlib.some_or "" audio_pipeline)
        (Stdlib.some_or "" video_pipeline)
    in
    let bin = Gstreamer.Pipeline.parse_launch pipeline in
    let audio_src =
      Gstreamer.App_src.of_element (Gstreamer.Bin.get_by_name bin "audio_src")
    in
    let video_src =
      Gstreamer.App_src.of_element (Gstreamer.Bin.get_by_name bin "video_src")
    in
    let sink = Gstreamer.App_sink.of_element (Gstreamer.Bin.get_by_name bin "sink") in
    Gstreamer.App_sink.on_new_sample sink on_sample;
    ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_playing);
    { bin; audio_src; video_src; sink }
  in

  let stop gst () =
    let ans = ref "" in
    ignore (Gstreamer.Element.set_state gst.bin Gstreamer.Element.State_null);
    ignore (Gstreamer.Element.get_state gst.bin);
    while !samples > 0 do
      let b = Gstreamer.App_sink.pull_buffer_string gst.sink in
      decr_samples ();
      ans := !ans ^ b
    done;
    !ans
  in

  let insert_metadata gst _ =
    (* TODO? *)
    ()
  in

  let now = ref Int64.zero in

  let encode h frame start len =
    let nanolen = Int64.of_float (Frame.seconds_of_master len *. 1000000000.) in
    let content = Frame.content_of_type frame start { Frame.audio = channels; video = 1; midi = 0 } in
    if channels > 0 then
     begin
      (* Put audio. *)
      let astart = Frame.audio_of_master start in
      let alen = Frame.audio_of_master len in
      let pcm = content.Frame.audio in
      let data = String.create (2*channels*alen) in
      Audio.S16LE.of_audio pcm astart data 0 alen;
      let gstbuf = Gstreamer.Buffer.of_string data 0 (String.length data) in
      Gstreamer.Buffer.set_presentation_time gstbuf !now;
      Gstreamer.Buffer.set_duration gstbuf nanolen;
      Gstreamer.App_src.push_buffer gst.audio_src gstbuf;
     end;
    (* Put video. *)
    let vbuf = content.Frame.video in
    let vbuf = vbuf.(0) in
    let vlen = Int64.div nanolen (Int64.of_int (Array.length vbuf)) in
    for i = 0 to Array.length vbuf - 1 do
      let data = Img.data vbuf.(i) in
      let gstbuf = Gstreamer.Buffer.of_data data 0 (Bigarray.Array1.dim data) in
      let ptime =
        Int64.add !now (Int64.mul (Int64.of_int i) vlen)
      in
      Gstreamer.Buffer.set_presentation_time gstbuf ptime;
      Gstreamer.Buffer.set_duration gstbuf vlen;
      Gstreamer.App_src.push_buffer gst.video_src gstbuf;
    done;
    (* Return result. *)
    now := Int64.add !now nanolen;
    if !samples = 0 then
      ""
    else
      let ans = Gstreamer.App_sink.pull_buffer_string gst.sink in
      decr_samples ();
      ans
  in

  {
    Encoder.
    insert_metadata = insert_metadata gst;
    (* TODO: can we get a header? *)
    header = None;
    encode = encode gst;
    stop   = stop gst;
  }

let () =
  Encoder.plug#register "GSTREAMER"
    (function
    | Encoder.GStreamer m -> Some (fun s _ -> encoder s m)
    | _ -> None)
