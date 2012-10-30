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
  let channels = 2 in

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
    Printf.printf "Got sample!\n%!";
    (* Will we finally get there????....... *)
    exit (-1);
    incr_samples();
  in

  let gst =
    let audio_pipeline =
      Printf.sprintf "%s ! queue ! %s ! lamemp3enc ! muxer."
        (GU.Pipeline.audio_src ~channels ~block:false "audio_src")
        (GU.Pipeline.convert_audio ())
    in
    let video_pipeline =
      Printf.sprintf "%s ! queue ! %s ! x264enc ! muxer."
        (GU.Pipeline.video_src ~block:true "video_src")
        (GU.Pipeline.convert_video ())
    in
    let pipeline =
      Printf.sprintf "%s %s avimux name=muxer ! appsink name=sink sync=false emit-signals=true"
        audio_pipeline
        video_pipeline
    in
    Printf.printf "pipeline: %s\n%!" pipeline;
    let bin = Gstreamer.Pipeline.parse_launch pipeline in
    let audio_src = Gstreamer.App_src.of_element (Gstreamer.Bin.get_by_name bin "audio_src") in
    let video_src = Gstreamer.App_src.of_element (Gstreamer.Bin.get_by_name bin "audio_src") in
    let sink = Gstreamer.App_sink.of_element (Gstreamer.Bin.get_by_name bin "sink") in
    Gstreamer.App_sink.on_new_sample sink on_sample;
    ignore (Gstreamer.Element.set_state bin Gstreamer.Element.State_playing);
    { bin; audio_src; video_src; sink }
  in

  let stop gst () =
    let ans = ref "" in
    (* TODO: send EOS on both appsrc *)
    ignore (Gstreamer.Element.set_state gst.bin Gstreamer.Element.State_paused);
    while !samples > 0 do
      let b = Gstreamer.App_sink.pull_buffer_string gst.sink in
      decr_samples ();
      ans := !ans ^ b
    done;
    ignore (Gstreamer.Element.set_state gst.bin Gstreamer.Element.State_null);
    !ans
  in

  let insert_metadata gst _ =
    (* TODO? *)
    ()
  in

  let now = ref Int64.zero in

  let encode h frame start len =
    Printf.printf "Encode@%Lu.\n%!" !now;
    let nanolen = Int64.of_float (Frame.seconds_of_master len *. 1000000000.) in
    let content = Frame.content_of_type frame start { Frame.audio = channels; video = 1; midi = 0 } in
    (* Put audio. *)
    let astart = Frame.audio_of_master start in
    let alen = Frame.audio_of_master len in
    let pcm = content.Frame.audio in
    let data = String.create (2*channels*alen) in
    Audio.S16LE.of_audio pcm astart data 0 alen;
    let gstbuf = Gstreamer.Buffer.of_string data 0 (String.length data) in
    Gstreamer.Buffer.set_presentation_time gstbuf !now;
    Gstreamer.Buffer.set_duration gstbuf nanolen;
    Printf.printf "Put audio... %!";
    Gstreamer.App_src.push_buffer gst.audio_src gstbuf;
    Printf.printf "done.\n%!";
    (* Put video. *)
    let vbuf = content.Frame.video in
    let vbuf = vbuf.(0) in
    for i = 0 to Array.length vbuf - 1 do
      let data = Img.data vbuf.(i) in
      let gstbuf = Gstreamer.Buffer.of_data data 0 (Bigarray.Array1.dim data) in
      Gstreamer.Buffer.set_presentation_time gstbuf !now;
      Gstreamer.Buffer.set_duration gstbuf nanolen;
      Printf.printf "Put video... %!";
      Gstreamer.App_src.push_buffer gst.video_src gstbuf;
      Printf.printf "done.\n%!"
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
