(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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
    let bin, audio_src, video_src = self#get_gst in
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

  method close = ()

  val mutable audio_now = Int64.zero
  val mutable video_now = Int64.zero
  val audio_buffer = Queue.create ()
  val video_buffer = Queue.create ()
  val audio_buffer_mutex = Mutex.create ()
  val video_buffer_mutex = Mutex.create ()
  val audio_buffer_condition = Condition.create ()
  val video_buffer_condition = Condition.create ()

  method feed_audio n =
    let bin, audio_src, video_src = self#get_gst in
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

  method feed_video n =
    let bin, audio_src, video_src = self#get_gst in
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

let input_proto =
  [
    "clock_safe",
    Lang.bool_t, Some (Lang.bool true),
    Some "Force the use of the dedicated GStreamer clock.";
  ]

(* Video *)

class video_input p kind pipeline =
  let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let () = GU.init () in
object (self)
  inherit Source.active_source ~name:"input.gstreamer.video" kind as active_source

  method private set_clock =
    active_source#set_clock;
    if clock_safe then
      Clock.unify self#clock
        (Clock.create_known ((gst_clock ()):>Clock.clock))

  method private wake_up l =
    active_source#wake_up l;
    if clock_safe then (gst_clock ())#register_blocking_source

  method private sleep =
    if clock_safe then (gst_clock ())#unregister_blocking_source

  method stype = Source.Fallible
  method remaining = -1
  val mutable ready = true
  method is_ready = ready

  method abort_track = ()

  val mutable gst = None

  method close =
    match gst with
    | Some (bin,sink) -> ignore (Element.set_state bin Element.State_null)
    | None -> ()

  method get_gst =
    match gst with
      | Some gst -> gst
      | None ->
        let pipeline =
          Printf.sprintf
            "%s ! %s ! %s"
            pipeline (GU.Pipeline.decode_video ())
            (GU.Pipeline.video_sink "sink")
        in
        let bin = Pipeline.parse_launch pipeline in
        let sink = App_sink.of_element (Bin.get_by_name bin "sink") in
        gst <- Some (bin,sink);
        try
          ignore (Element.set_state bin Element.State_playing);
          bin,sink
        with
          |e ->
              ignore (Element.set_state bin Element.State_null);
              raise e

  method output = if AFrame.is_partial memo then self#get_frame memo
  method output_get_ready = ()
  method output_reset = ()
  method is_active = true

  method get_frame frame =
    let bin, sink = self#get_gst in
    assert (0 = Frame.position frame);
    let buf = VFrame.content_of_type ~channels:1 frame in
    let buf = buf.(0) in
    let i = ref 0 in
    try
      while !i < Array.length buf do
        let b = App_sink.pull_buffer_data sink in
        let img = Img.make width height b in
        buf.(!i) <- img;
        incr i;
      done;
      VFrame.add_break frame !i
    with
    | Gstreamer.End_of_stream ->
      VFrame.add_break frame !i;
      ready <- false
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.video_n 1) in
  let proto = input_proto@
    [
      "pipeline", Lang.string_t, Some (Lang.string "videotestsrc"),
      Some "GStreamer pipeline to input from.";
    ]
  in
  Lang.add_operator "input.gstreamer.video" proto ~active:true
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[]
    ~descr:"Stream video from GStreamer pipeline."
    (fun p kind ->
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      ((new video_input p kind pipeline):>Source.source))

(* Audio + video. *)

module Generator = Generator.From_audio_video_plus

class audio_video_input p kind (pipeline,audio_pipeline,video_pipeline) =
  let has_video, video_pipeline =
    match video_pipeline with
    | None -> false, ""
    | Some video_pipeline -> true, video_pipeline
  in
  let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let rlog = ref (fun _ -> ()) in
  let gen = Generator.create ~log:(fun x -> !rlog x) ~kind (if has_video then `Both else `Audio) in
  let () = GU.init () in
object (self)
  inherit Source.active_source ~name:"input.gstreamer.audio_video" kind as super

  initializer
    rlog := (fun s -> self#log#f 3 "%s" s)

  method private set_clock =
    super#set_clock;
    if clock_safe then
      Clock.unify self#clock
        (Clock.create_known ((gst_clock ()):>Clock.clock))

  method private wake_up l =
    super#wake_up l;
    if clock_safe then
      (gst_clock ())#register_blocking_source

  method private sleep =
    if clock_safe then
      (gst_clock ())#unregister_blocking_source

  method stype = Source.Fallible
  method remaining = -1
  val mutable ready = true
  method is_ready = ready

  method abort_track = ()

  val mutable gst = None

  val mutable bin = None

  method close =
    match bin with
    | Some bin -> ignore (Element.set_state bin Element.State_null)
    | None -> ()

  method get_device =
    match gst with
    | Some gst -> gst
    | None ->
      let pipeline =
        Printf.sprintf
          "%s %s ! %s ! %s"
          pipeline
          audio_pipeline
          (GU.Pipeline.decode_audio ())
          (GU.Pipeline.audio_sink ~channels "audio_sink")
      in
      let pipeline =
        if has_video then
          Printf.sprintf
            "%s %s ! %s ! %s"
            pipeline
            video_pipeline
            (GU.Pipeline.decode_video ())
            (GU.Pipeline.video_sink "video_sink")
        else
          pipeline
      in
      log#f 5 "GStreamer pipeline: %s" pipeline;
      bin <- Some (Pipeline.parse_launch pipeline);
      let bin = Utils.get_some bin in
      let audio_sink = App_sink.of_element (Bin.get_by_name bin "audio_sink") in
      let video_sink =
        if has_video then
          App_sink.of_element (Bin.get_by_name bin "video_sink")
        else
          (* This is hacky... *)
          audio_sink
      in
      let sinks = audio_sink, video_sink in
      gst <- Some sinks;
      try
        ignore (Element.set_state bin Element.State_playing);
        sinks
      with
        | e ->
            ignore (Element.set_state bin Element.State_null);
            raise e

  method output_get_ready = ()
  method output_reset = ()
  method is_active = true

  method fill_audio =
    let audio_sink, video_sink = self#get_device in
    let b = Gstreamer.App_sink.pull_buffer_string audio_sink in
    let len = String.length b / (2*channels) in
    let buf = Audio.create channels len in
    Audio.S16LE.to_audio b 0 buf 0 len;
    Generator.put_audio gen buf 0 len

  method fill_video =
    let audio_sink, video_sink = self#get_device in
    let b = Gstreamer.App_sink.pull_buffer_data video_sink in
    let img = Img.make width height b in
    let stream = [|img|] in
    Generator.put_video gen [|stream|] 0 (Array.length stream)

  method get_frame frame =
    try
      while Generator.audio_length gen < AFrame.size () do
        self#fill_audio
      done;
      if has_video then
        while Generator.video_length gen < VFrame.size () do
          self#fill_video
        done;
      Generator.fill gen frame
    with
    | Gstreamer.End_of_stream -> ready <- false

  method output = if AFrame.is_partial memo then self#get_frame memo
end

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
  let proto = input_proto@
    [
      "pipeline", Lang.string_t, Some (Lang.string ""),
      Some "Main GStreamer pipeline.";
      "audio_pipeline", Lang.string_t, Some (Lang.string "audiotestsrc"),
      Some "Audio pipeline to input from.";
      "video_pipeline", Lang.string_t, Some (Lang.string "videotestsrc"),
      Some "Video pipeline to input from.";
    ]
  in
  Lang.add_operator "input.gstreamer.audio_video" proto ~active:true
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[]
    ~descr:"Stream audio+video from a GStreamer pipeline."
    (fun p kind ->
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      let audio_pipeline = Lang.to_string (List.assoc "audio_pipeline" p) in
      let video_pipeline = Lang.to_string (List.assoc "video_pipeline" p) in
      ((new audio_video_input p kind (pipeline,audio_pipeline,Some video_pipeline)):>Source.source))

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
  let proto = input_proto@
    [
      "pipeline", Lang.string_t, Some (Lang.string "audiotestsrc"),
      Some "GStreamer pipeline to input from.";
    ]
  in
  Lang.add_operator "input.gstreamer.audio" proto ~active:true
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~flags:[]
    ~descr:"Stream video from GStreamer pipeline."
    (fun p kind ->
      let pipeline = Lang.to_string (List.assoc "pipeline" p) in
      ((new audio_video_input p kind ("",pipeline,None)):>Source.source))
