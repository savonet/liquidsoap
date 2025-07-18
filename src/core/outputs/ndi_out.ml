(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** Output using NDI. *)

open Mm
open Ndi_format

module SyncSource = Clock.MkSyncSource (struct
  type t = unit

  let to_string _ = "ndi"
end)

let sync_source = SyncSource.make ()

type sender = { handler : Ndi.Send.sender; mutable position : int64 }

class output ~self_sync ~register_telnet ~name ~groups ~infallible ~handler
  ~format source start =
  let sample_rate = Lazy.force Frame.audio_rate in
  let frame_rate = Lazy.force Frame.video_rate in
  let video_height = Lazy.force Frame.video_height in
  let video_width = Lazy.force Frame.video_width in
  (* Timecode is in increment of 100 ns *)
  let timecode_base =
    Int64.div 10_000_000L (Int64.of_int (Lazy.force Frame.main_rate))
  in
  let clock_audio, clock_video =
    match (self_sync, format.audio, format.video) with
      | false, _, _ -> (false, false)
      | true, _, true -> (false, true)
      | true, true, _ -> (true, false)
      | _ -> assert false
  in
  object (self)
    inherit
      Output.output
        ~register_telnet ~infallible ~name:"ndi" ~output_kind:"output.ndi"
          source start

    val mutable sender = None

    method self_sync =
      if self_sync then
        (`Dynamic, if sender <> None then Some sync_source else None)
      else (`Static, None)

    method get_sender =
      match sender with
        | Some s -> s
        | None ->
            let handler =
              Ndi.Send.init ~clock_audio ~clock_video ?groups ?name handler
            in
            let s = { handler; position = 0L } in
            sender <- Some s;
            s

    method start = ignore self#get_sender

    method stop =
      match sender with
        | Some { handler } ->
            Ndi.Send.destroy handler;
            sender <- None
        | None -> ()

    method private send_audio_frame ~timecode ~sender frame =
      let pcm = AFrame.pcm frame in
      let channels = Array.length pcm in
      let samples = Audio.length pcm in
      let data =
        Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
          (samples * channels)
      in
      Audio.FLTP.of_audio ~src:pcm ~src_offset:0 ~dst:data ~dst_offset:0
        ~len:samples ~stride:samples;
      let audio_frame =
        {
          Ndi.Frame.Audio.sample_rate;
          channels;
          samples;
          timecode = Some timecode;
          data = `Fltp { Ndi.Frame.Audio.data; stride = samples * 4 };
          metadata = None;
          timestamp = None;
        }
      in
      Ndi.Send.send_audio sender.handler audio_frame

    method private send_video_frame ~timecode ~sender frame =
      let buf = VFrame.data frame in
      List.iter
        (fun (pos, img) ->
          let img =
            img
            (* TODO: we could scale instead of aggressively changing the viewport *)
            |> Video.Canvas.Image.viewport video_width video_height
            |> Video.Canvas.Image.render ~transparent:false
          in
          let y, u, v = Image.YUV420.data img in
          let y_dim = Bigarray.Array1.dim y in
          let u_dim = Bigarray.Array1.dim u in
          let v_dim = Bigarray.Array1.dim v in
          let stride = Image.YUV420.y_stride img in
          let data =
            Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
              (y_dim + u_dim + v_dim)
          in
          Bigarray.Array1.blit y (Bigarray.Array1.sub data 0 y_dim);
          Bigarray.Array1.blit u (Bigarray.Array1.sub data y_dim u_dim);
          Bigarray.Array1.blit v
            (Bigarray.Array1.sub data (y_dim + u_dim) v_dim);
          let video_frame =
            {
              Ndi.Frame.Video.xres = video_width;
              yres = video_height;
              frame_rate_N = frame_rate;
              frame_rate_D = 1;
              picture_aspect_ratio = None;
              format = `Progressive;
              timecode =
                Some Int64.(add timecode (mul (of_int pos) timecode_base));
              data = `I420 { Ndi.Frame.Video.data; stride };
              metadata = None;
              timestamp = None;
            }
          in
          Ndi.Send.send_video sender.handler video_frame)
        buf.Content.Video.data

    method send_frame frame =
      let sender = self#get_sender in
      let timecode = Int64.mul sender.position timecode_base in
      if format.audio then self#send_audio_frame ~timecode ~sender frame;
      if format.video then self#send_video_frame ~timecode ~sender frame;
      sender.position <-
        Int64.add sender.position (Int64.of_int (Frame.position frame))

    method! reset = ()
  end

let _ =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:Modules.output "ndi" ~flags:[`Experimental]
    (Output.proto
    @ [
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool false),
          Some "Use the dedicated NDI clock." );
        ( "library_file",
          Lang.string_t,
          None,
          Some "Path to the shared library file." );
        ( "name",
          Lang.nullable_t Lang.string_t,
          Some Lang.null,
          Some "NDI sender name" );
        ( "groups",
          Lang.nullable_t Lang.string_t,
          Some Lang.null,
          Some "NDI sender groups" );
        ( "",
          Lang.format_t return_t,
          None,
          Some "Encoding format. Only the `%ndi` encoder is allowed here!" );
        ("", Lang.source_t return_t, None, None);
      ])
    ~category:`Output ~meth:Output.meth ~callbacks:Output.callbacks
    ~descr:"Output stream to NDI" ~return_t
    (fun p ->
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let lib = Lang.to_string (List.assoc "library_file" p) in
      let lib = Utils.check_readable ~pos:(Lang.pos p) lib in
      let handler =
        try Ndi.init ~filename:lib () with
          | Ndi.Library_not_found ->
              Runtime_error.raise ~pos:(Lang.pos p)
                ~message:"Invalid ndi library" "invalid"
          | Ndi.Library_initialized f ->
              Runtime_error.raise ~pos:(Lang.pos p)
                ~message:
                  (Printf.sprintf
                     "Ndi already initialized with a different library: %s"
                     (Lang_string.quote_string f))
                "invalid"
      in
      let name = Lang.to_valued_option Lang.to_string (List.assoc "name" p) in
      let groups =
        Lang.to_valued_option Lang.to_string (List.assoc "groups" p)
      in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let format =
        match Lang.to_format (Lang.assoc "" 1 p) with
          | NDI n -> n
          | _ ->
              Runtime_error.raise ~pos:(Lang.pos p)
                ~message:"Only the %ndi encoder is allowed for `output.ndi`!"
                "invalid"
      in
      let source = Lang.assoc "" 2 p in
      (new output
         ~self_sync ~name ~groups ~infallible ~register_telnet ~handler ~format
         source start
        :> Output.output))
