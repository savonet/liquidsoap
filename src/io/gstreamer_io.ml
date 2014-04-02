(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2014 Savonet team

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

open Stdlib
open Gstreamer

module GU = Gstreamer_utils
module Img = Image.RGBA32

let log = Dtools.Log.make ["io";"gstreamer"]
let gst_clock = Tutils.lazy_cell (fun () -> new Clock.self_sync "gstreamer")

let string_of_state_change = function
  | Element.State_change_success    -> "success"
  | Element.State_change_async      -> "asynchronous"
  | Element.State_change_no_preroll -> "no pre-roll"

(***** Output *****)

(* Audio/video output *)

class output ~kind ~clock_safe
  ~infallible ~on_start ~on_stop ?(blocking=true)
  source start (pipeline,audio_pipeline,video_pipeline)
  =
  let has_audio, audio_pipeline =
    match audio_pipeline with
    | Some audio_pipeline -> true, audio_pipeline
    | None -> false, ""
  in
  let has_video, video_pipeline =
    match video_pipeline with
    | Some video_pipeline -> true, video_pipeline
    | None -> false, ""
  in
  let channels = (Frame.type_of_kind kind).Frame.audio in
object (self)
  inherit Output.output  ~content_kind:kind
    ~infallible ~on_start ~on_stop
    ~name:"output.gstreamer" ~output_kind:"gstreamer" source start as super

  method private set_clock =
    super#set_clock;
    if clock_safe then
      Clock.unify self#clock
        (Clock.create_known ((gst_clock ()):>Clock.clock))

  method output_start =
    let bin,_,_ = self#get_gst in
    begin
     try
      ignore (Element.set_state bin Element.State_playing)
     with e ->
       ignore (Element.set_state bin Element.State_null);
       raise e
    end;
    if clock_safe then (gst_clock ())#register_blocking_source

  method output_stop =
    let bin,audio_src,video_src = self#get_gst in
    if has_audio then
      App_src.end_of_stream (Utils.get_some audio_src);
    if has_video then
      App_src.end_of_stream (Utils.get_some video_src);
    ignore (Element.set_state bin Element.State_null);
    if clock_safe then (gst_clock ())#unregister_blocking_source

  val mutable gst = None

  method private get_gst =
    match gst with
    | Some gst -> gst
    | None ->
      GU.init ();
      let pipeline =
        if has_audio then
          Printf.sprintf "%s ! %s %s"
            (GU.Pipeline.audio_src ~channels ~block:blocking "audio_src")
            audio_pipeline
            pipeline
        else
          pipeline
      in
      let pipeline =
        if has_video then
          Printf.sprintf "%s ! %s %s"
            (GU.Pipeline.video_src ~block:blocking "video_src")
            video_pipeline
            pipeline
        else
          pipeline
      in
      self#log#f 5 "GStreamer pipeline: %s" pipeline;
      let bin = Pipeline.parse_launch pipeline in
      let audio_src =
        if has_audio then
          let audio_src = App_src.of_element (Bin.get_by_name bin "audio_src") in
          Some audio_src
        else
          None
      in
      let video_src =
        if has_video then
          let video_src = App_src.of_element (Bin.get_by_name bin "video_src") in
          Some video_src
        else
          None
      in
      gst <- Some (bin, audio_src, video_src);
      self#get_gst

  val mutable now = Int64.zero

  method output_send frame =
    let _, audio_src, video_src = self#get_gst in
    if not (Frame.is_partial frame) then
      let _, content = Frame.content frame 0 in
      let len = Lazy.force Frame.size in
      let nanolen = Int64.of_float (Frame.seconds_of_master len *. 1000000000.) in
      if has_audio then
        (
          let pcm = content.Frame.audio in
          assert (Array.length pcm = channels);
          let len = Frame.audio_of_master len in
          let data = String.create (2*channels*len) in
          Audio.S16LE.of_audio pcm 0 data 0 len;
          let gstbuf = Gstreamer.Buffer.of_string data 0 (String.length data) in
          Gstreamer.Buffer.set_presentation_time gstbuf now;
          Gstreamer.Buffer.set_duration gstbuf nanolen;
          Gstreamer.App_src.push_buffer (Utils.get_some audio_src) gstbuf
        );
      if has_video then
        (
          let buf = content.Frame.video.(0) in
          for i = 0 to Array.length buf - 1 do
            let data = Img.data buf.(i) in
            let gstbuf = Gstreamer.Buffer.of_data data 0 (Bigarray.Array1.dim data) in
            Gstreamer.Buffer.set_presentation_time gstbuf now;
            Gstreamer.Buffer.set_duration gstbuf nanolen;
            Gstreamer.App_src.push_buffer (Utils.get_some video_src) gstbuf
          done;
        );
      now <- Int64.add now nanolen

  method output_reset = ()
end

let () =
  let kind =
    Lang.any_fixed_with ~audio:1 ()
  in
  let kind = Lang.kind_type_of_kind_format ~fresh:1 kind in
  Lang.add_operator "output.gstreamer.audio" ~active:true
    ( Output.proto @
        [ "clock_safe",
          Lang.bool_t, Some (Lang.bool true),
          Some "Use the dedicated GStreamer clock." ;
          "pipeline",
          Lang.string_t, Some (Lang.string "autoaudiosink"),
          Some "GStreamer pipeline for sink.";
          "", Lang.source_t kind, None, None
        ])
    ~category:Lang.Output
    ~descr:"Output stream to a GStreamer pipeline."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let source = List.assoc "" p in
      (new output ~kind ~clock_safe ~infallible ~on_start ~on_stop source start ("",Some pipeline,None) :> Source.source)
    )

let () =
  let kind =
    Lang.any_fixed_with ~video:1 ()
  in
  let kind = Lang.kind_type_of_kind_format ~fresh:1 kind in
  Lang.add_operator "output.gstreamer.video" ~active:true
    ( Output.proto @
        [ "clock_safe",
          Lang.bool_t, Some (Lang.bool true),
          Some "Use the dedicated GStreamer clock.";
          "pipeline",
          Lang.string_t, Some (Lang.string "videoconvert ! autovideosink"),
          Some "GStreamer pipeline for sink.";
          "", Lang.source_t kind, None, None
        ])
    ~category:Lang.Output
    ~descr:"Output stream to a GStreamer pipeline."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let source = List.assoc "" p in
      (new output ~kind ~clock_safe ~infallible ~on_start ~on_stop source start ("",None,Some pipeline) :> Source.source)
    )

(* An asynchronous version of the output. The problem with the above version is
   that push_buffer is blocking and we sometimes have deadlocks when audio and
   video are not synced. *)
class output_audio_video ~kind ~clock_safe
  ~infallible ~on_start ~on_stop
  source start (pipeline,audio_pipeline,video_pipeline)
  =
  let channels = (Frame.type_of_kind kind).Frame.audio in
object (self)
  inherit Output.output  ~content_kind:kind
    ~infallible ~on_start ~on_stop
    ~name:"output.gstreamer_audio_video" ~output_kind:"gstreamer" source start as super

  method private set_clock =
    super#set_clock;
    if clock_safe then
      Clock.unify self#clock
        (Clock.create_known ((gst_clock ()):>Clock.clock))

  method output_start =
    let bin,_,_ = self#get_gst in
    begin
     try
      ignore (Element.set_state bin Element.State_playing)
     with
       | e ->
           ignore (Element.set_state bin Element.State_null);
           raise e
    end;
    if clock_safe then (gst_clock ())#register_blocking_source

  method output_stop =
    let bin,audio_src,video_src = self#get_gst in
    App_src.end_of_stream audio_src;
    App_src.end_of_stream video_src;
    ignore (Element.set_state bin Element.State_null);
    if clock_safe then (gst_clock ())#unregister_blocking_source

  val mutable gst = None

  method private get_gst =
    match gst with
    | Some gst -> gst
    | None ->
      GU.init ();
      let pipeline =
        Printf.sprintf "%s ! %s %s ! %s %s"
          (GU.Pipeline.audio_src ~channels "audio_src")
          audio_pipeline
          (GU.Pipeline.video_src "video_src")
          video_pipeline
          pipeline
      in
      self#log#f 5 "GStreamer pipeline: %s" pipeline;
      let bin = Pipeline.parse_launch pipeline in
      let audio_src = App_src.of_element (Bin.get_by_name bin "audio_src") in
      let video_src = App_src.of_element (Bin.get_by_name bin "video_src") in
      App_src.on_need_data audio_src self#feed_audio;
      App_src.on_need_data video_src self#feed_video;
      gst <- Some (bin, audio_src, video_src);
      self#get_gst

  val mutable audio_now = Int64.zero
  val mutable video_now = Int64.zero
  val audio_buffer = Queue.create ()
  val video_buffer = Queue.create ()
  val audio_buffer_mutex = Mutex.create ()
  val video_buffer_mutex = Mutex.create ()
  val audio_buffer_condition = Condition.create ()
  val video_buffer_condition = Condition.create ()

  method feed_audio _ =
    let _, audio_src, _ = self#get_gst in
    Tutils.mutexify audio_buffer_mutex (fun () ->
      while Queue.is_empty audio_buffer do
        Condition.wait audio_buffer_condition audio_buffer_mutex
      done;
      let data = Queue.pop audio_buffer in
      let len = String.length data in
      let nanolen = Int64.of_float (Frame.seconds_of_audio (len / (2 * channels)) *. 1000000000.) in
      let gstbuf = Gstreamer.Buffer.of_string data 0 len in
      Gstreamer.Buffer.set_presentation_time gstbuf audio_now;
      Gstreamer.Buffer.set_duration gstbuf nanolen;
      Gstreamer.App_src.push_buffer audio_src gstbuf;
      audio_now <- Int64.add audio_now nanolen) ()

  method feed_video _ =
    let _, _, video_src = self#get_gst in
    Tutils.mutexify video_buffer_mutex (fun () ->
      while Queue.is_empty video_buffer do
        Condition.wait video_buffer_condition video_buffer_mutex
      done;
      let img = Queue.pop video_buffer in
      let data = Img.data img in
      let len = Bigarray.Array1.dim data in
      let nanolen = Int64.of_float (Frame.seconds_of_video 1 *. 1000000000.) in
      let gstbuf = Gstreamer.Buffer.of_data data 0 len in
      Gstreamer.Buffer.set_presentation_time gstbuf video_now;
      Gstreamer.Buffer.set_duration gstbuf nanolen;
      Gstreamer.App_src.push_buffer video_src gstbuf;
      video_now <- Int64.add video_now nanolen) ()

  method output_send frame =
    if not (Frame.is_partial frame) then
      let _, content = Frame.content frame 0 in
      let len = Lazy.force Frame.size in

      (* Read audio. *)
      let pcm = content.Frame.audio in
      assert (Array.length pcm = channels);
      let len = Frame.audio_of_master len in
      let data = String.create (2*channels*len) in
      Audio.S16LE.of_audio pcm 0 data 0 len;
      Tutils.mutexify audio_buffer_mutex (fun () ->
        Queue.push data audio_buffer;
        Condition.signal audio_buffer_condition) ();

      (* Read video. *)
      let buf = content.Frame.video.(0) in
      Tutils.mutexify video_buffer_mutex (fun () ->
        for i = 0 to Array.length buf - 1 do
          Queue.push buf.(i) video_buffer
        done;
        Condition.signal video_buffer_condition) ()

  method output_reset = ()
end

let () =
  let kind =
    Lang.any_fixed_with ~audio:1 ~video:1 ()
  in
  let kind = Lang.kind_type_of_kind_format ~fresh:1 kind in
  Lang.add_operator "output.gstreamer.audio_video" ~active:true
    ( Output.proto @
        [ "clock_safe",
          Lang.bool_t, Some (Lang.bool true),
          Some "Use the dedicated GStreamer clock.";
          "audio_pipeline",
          Lang.string_t, Some (Lang.string "autoaudiosink"),
          Some "GStreamer pipeline for audio sink.";
          "video_pipeline",
          Lang.string_t, Some (Lang.string "videoconvert ! autovideosink"),
          Some "GStreamer pipeline for video sink.";
          "pipeline",
          Lang.string_t, Some (Lang.string ""),
          Some "GStreamer pipeline for sink.";
          "blocking",
          Lang.bool_t, Some (Lang.bool true),
          Some "Pushing buffers is blocking.";
          "asynchronous",
          Lang.bool_t, Some (Lang.bool false),
          Some "Use asynchronous implementation (GStreamer is pulling from us).";
          "", Lang.source_t kind, None, None
        ])
    ~category:Lang.Output
    ~descr:"Output stream to a GStreamer pipeline."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      let audio_pipeline = Lang.to_string (List.assoc "audio_pipeline" p) in
      let video_pipeline = Lang.to_string (List.assoc "video_pipeline" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let blocking = Lang.to_bool (List.assoc "blocking" p) in
      let asynchronous = Lang.to_bool (List.assoc "asynchronous" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let source = List.assoc "" p in
      if asynchronous then
        (new output_audio_video ~kind ~clock_safe ~infallible ~on_start ~on_stop source start (pipeline,audio_pipeline,video_pipeline) :> Source.source)
      else
        (new output ~kind ~clock_safe ~infallible ~on_start ~on_stop ~blocking source start (pipeline,Some audio_pipeline,Some video_pipeline) :> Source.source)
    )

(***** Input *****)

(* Audio + video. *)

module Generator = Generator.From_audio_video_plus

type 'a sink = {
  pending : unit -> int;
  pull    : unit -> 'a
}

type element = {
  bin   : Gstreamer.Element.t;
  audio : string sink option;
  video : Gstreamer.data sink option
}

class audio_video_input p kind (pipeline,audio_pipeline,video_pipeline) =
  let max = Lang.to_float (List.assoc "max" p) in
  let max_ticks = Frame.master_of_seconds max in
  let content,has_audio,has_video =
    match audio_pipeline, video_pipeline with
     | Some _, Some _ -> `Both,true,true
     | None,   Some _ -> `Video,false,true
     | Some _, None   -> `Audio,true,false
     | None,   None   -> failwith "There should be at least one audio or video pipeline!"
  in
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let rlog = ref (fun _ -> ()) in
  let gen =
    Generator.create
      ~log:(fun x -> !rlog x) ~kind
      content 
  in
  let () = GU.init () in
object (self)
  inherit Source.source ~name:"input.gstreamer.audio_video" kind as super

  initializer
    rlog := (fun s -> self#log#f 3 "%s" s);
    let change_state s _ =
      try
        Printf.sprintf "Done. State change returned: %s"
          (string_of_state_change (Element.set_state self#get_device.bin s) )
      with
        | e ->
            Printf.sprintf "Error while changing state: %s\n" (Printexc.to_string e)
    in
    self#register_command
      "pause" ~descr:"Set gstreamer pipeline state to paused" (change_state Element.State_paused);
    self#register_command
      "play" ~descr:"Set gstreamer pipeline state to playing" (change_state Element.State_playing)

  method stype = Source.Fallible
  method remaining = -1

  (* Source is ready when ready = true
   * and gst has some audio or some video. *)
  val mutable ready = true
  method is_ready =
    let pending = function
      | Some sink -> sink.pending() > 0
      | None      -> false
    in
    try
      ready &&
        ((Generator.length gen > 0) ||
         (pending self#get_device.audio) ||
         (pending self#get_device.video))
    with
    | e ->
      log#f 4 "Error when trying checking if ready: %s" (Printexc.to_string e);
      false

  method abort_track = ()

  val mutable gst = None

  method sleep =
   begin
    match gst with
      | Some gst -> ignore (Element.set_state gst.bin Element.State_null)
      | None -> ()
   end;
   super#sleep

  method get_device =
    match gst with
      | Some gst -> gst
      | None ->
        let pipeline =
          if has_audio then
            Printf.sprintf
              "%s %s ! %s ! %s"
              pipeline
              (Utils.get_some audio_pipeline)
              (GU.Pipeline.decode_audio ())
              (GU.Pipeline.audio_sink ~channels "audio_sink")
          else
            pipeline
        in
        let pipeline =
          if has_video then
            Printf.sprintf
              "%s %s ! %s ! %s"
              pipeline
              (Utils.get_some video_pipeline)
              (GU.Pipeline.decode_video ())
              (GU.Pipeline.video_sink "video_sink")
          else
            pipeline
        in
        log#f 5 "GStreamer pipeline: %s" pipeline;
        let bin =  Pipeline.parse_launch pipeline in
        let wrap_sink sink pull =
          let m = Mutex.create () in
          let counter = ref 0 in
          App_sink.emit_signals sink;
          App_sink.on_new_sample sink (Tutils.mutexify m (fun () ->
            incr counter));
          let pending = Tutils.mutexify m (fun () ->
            !counter)
          in
          let pull = Tutils.mutexify m (fun () ->
            let b = pull sink in
            decr counter;
            b)
          in
          { pending = pending; pull = pull }
        in
        let audio_sink =
          if has_audio then
           begin
            let sink = App_sink.of_element (Bin.get_by_name bin "audio_sink") in
            Some (wrap_sink sink Gstreamer.App_sink.pull_buffer_string)
           end
          else
            None
        in
        let video_sink =
          if has_video then
           begin
            let sink = 
              App_sink.of_element (Bin.get_by_name bin "video_sink")
            in
            Some (wrap_sink sink Gstreamer.App_sink.pull_buffer_data)
           end
          else
            None
        in
        let element = {
          bin   = bin;
          audio = audio_sink;
          video = video_sink }
        in
        gst <- Some element;
        try
          ignore (Element.set_state bin Element.State_playing);
          element
        with
          | e ->
              ignore (Element.set_state bin Element.State_null);
              raise e

  method is_active = true

  method private is_generator_at_max =
    Generator.length gen >= max_ticks

  method private fill_audio =
    match self#get_device.audio with
      | None -> ()
      | Some audio ->
         while audio.pending () > 0 && not self#is_generator_at_max do
           let b = audio.pull() in
           let len = String.length b / (2*channels) in
           let buf = Audio.create channels len in
           Audio.S16LE.to_audio b 0 buf 0 len;
           Generator.put_audio gen buf 0 len
         done

  method private fill_video =
    match self#get_device.video with
      | None -> ()
      | Some video ->
         while video.pending () > 0 && not self#is_generator_at_max do
           let b = video.pull () in
           let img = Img.make width height b in
           let stream = [|img|] in
           Generator.put_video gen [|stream|] 0 (Array.length stream)
         done

  method get_frame frame =
    try
      self#fill_audio;
      self#fill_video;
      Generator.fill gen frame
    with
      | Gstreamer.End_of_stream ->
         ready <- false
end

let input_proto =
  [
    "max", Lang.float_t, Some (Lang.float 10.),
    Some "Maximum duration of the buffered data." ;
  ]

let () =
  let k =
    Lang.kind_type_of_kind_format ~fresh:1
      (Lang.Constrained
         { Frame.
           (* TODO: be more flexible on audio *)
           audio = Lang.Fixed 2;
           video = Lang.Fixed 1;
           midi = Lang.Fixed 0 })
  in
  let proto = input_proto @ 
    [
      "pipeline", Lang.string_t, Some (Lang.string ""),
      Some "Main GStreamer pipeline.";
      "audio_pipeline", Lang.string_t, Some (Lang.string "audiotestsrc"),
      Some "Audio pipeline to input from.";
      "video_pipeline", Lang.string_t, Some (Lang.string "videotestsrc"),
      Some "Video pipeline to input from.";
    ]
  in
  Lang.add_operator "input.gstreamer.audio_video" proto
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[]
    ~descr:"Stream audio+video from a GStreamer pipeline."
    (fun p kind ->
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      let audio_pipeline = Lang.to_string (List.assoc "audio_pipeline" p) in
      let video_pipeline = Lang.to_string (List.assoc "video_pipeline" p) in
      ((new audio_video_input p kind (pipeline,Some audio_pipeline,Some video_pipeline)):>Source.source))

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
  let proto = input_proto @ 
    [
      "pipeline", Lang.string_t, Some (Lang.string "audiotestsrc"),
      Some "GStreamer pipeline to input from.";
    ]
  in
  Lang.add_operator "input.gstreamer.audio" proto
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[]
    ~descr:"Stream audio from a GStreamer pipeline."
    (fun p kind ->
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      ((new audio_video_input p kind ("",Some pipeline,None)):>Source.source))

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.video_n 1) in
  let proto = input_proto @ 
    [
      "pipeline", Lang.string_t, Some (Lang.string "videotestsrc"),
      Some "GStreamer pipeline to input from.";
    ]
  in
  Lang.add_operator "input.gstreamer.video" proto
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[]
    ~descr:"Stream video from a GStreamer pipeline."
    (fun p kind ->
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      ((new audio_video_input p kind ("",None,Some pipeline)):>Source.source))
