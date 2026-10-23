(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let create ~output ~faults ~(log : Log.t) ~source_id ~width ~height ~alpha
    (stream : Stream_description.stream) params =
  let time_base = stream.time_base in
  let source_id = ref source_id in
  let target _ = if alpha then `Yuva420p else `Yuv420p in
  let converter =
    Internal_video_converter.create ~log ~width ~height ~target ()
  in
  let pixel_aspect = Display.pixel_aspect params in
  let display = Display.create ~params ~time_base () in
  let fps = { Decoder.num = Lazy.Mutexed.force Frame.video_rate; den = 1 } in
  let put image _ = output.Per_stream.put_image ~fps image in
  let convert frame =
    let format = Avutil.Video.frame_format frame in
    let format =
      match format.pixel_aspect with
        | None -> { format with pixel_aspect }
        | Some _ -> format
    in
    Internal_video_converter.convert ~source_id:!source_id ~format converter
      ~time_base ~put frame
  in
  let push ~position:_ (item : Per_stream.item) =
    match item with
      | `Video_frame frame -> (
          try
            Display.convert display convert frame;
            (match Avutil.Frame.metadata frame with
              | [] -> ()
              | m -> output.add_metadata (Frame.Metadata.from_list m));
            Faults.delivered faults;
            `Ok
          with
            | Avutil.Error e ->
                `Item_fault
                  (Printf.sprintf "Video frame dropped: %s"
                     (Avutil.string_of_error e))
            | Failure message ->
                `Item_fault (Printf.sprintf "Video frame dropped: %s" message))
      | _ -> `Item_fault "Unexpected item on a decoded video stream"
  in
  let flush () =
    Display.eof display convert;
    Internal_video_converter.flush converter ~put
  in
  {
    Per_stream.sparse = false;
    push;
    switch = (fun ~source_id:id ~continuing:_ -> source_id := id);
    advance = (fun _ -> ());
    flush;
  }
