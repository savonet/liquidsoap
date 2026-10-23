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

let accessors () =
  {
    Duration.get_ts =
      (fun frame ->
        match Avutil.Frame.pts frame with
          | Some _ as pts -> pts
          | None -> Avutil.Frame.best_effort_timestamp frame);
    set_ts = (fun frame ts -> Avutil.Frame.set_pts frame ts);
    get_duration = Avutil.Frame.duration;
  }

(* Frames into chunks on the field: one duration helper per time base, a
   replacement starting where the replaced one ended (common.md §6.4). *)
type 'item chunker = {
  push :
    time_base:Avutil.rational ->
    position:int option ->
    'item ->
    Per_stream.outcome;
  drain : unit -> unit;
  set_source : Source_id.t -> unit;
}

let chunker ~faults ~put
    ~(make_chunk :
       source_id:Source_id.t ->
       time_base:Avutil.rational ->
       length:int ->
       'item list ->
       Content.data) () =
  let source_id = ref Source_id.none in
  let helper = ref None in
  let filled = ref 0 in
  let emit time_base = function
    | Some { Duration.length; items } when length > 0 ->
        filled := !filled + length;
        put (make_chunk ~source_id:!source_id ~time_base ~length items)
    | _ -> ()
  in
  let drain () =
    match !helper with
      | Some (time_base, h) ->
          helper := None;
          emit time_base (Duration.flush h)
      | None -> ()
  in
  let gap ~time_base length =
    if length > 0 then (
      filled := !filled + length;
      put (make_chunk ~source_id:!source_id ~time_base ~length []))
  in
  let helper_for ~time_base ~position =
    match !helper with
      | Some (tb, h) when tb = time_base -> h
      | _ ->
          drain ();
          let start = Option.value ~default:!filled position in
          gap ~time_base (start - !filled);
          let h =
            Duration.create ~start:(max start !filled) ~convert_ts:false
              ~src:time_base ~faults (accessors ())
          in
          helper := Some (time_base, h);
          h
  in
  let push ~time_base ~position item : Per_stream.outcome =
    match Duration.push (helper_for ~time_base ~position) item with
      | `Chunk chunk ->
          emit time_base (Some chunk);
          `Ok
      | `Nothing | `Dropped -> `Ok
      | `Discontinuity reason ->
          drain ();
          `Source_fault reason
  in
  { push; drain; set_source = (fun id -> source_id := id) }

let fault message = function
  | Avutil.Error e ->
      `Item_fault (Printf.sprintf "%s: %s" message (Avutil.string_of_error e))
  | Failure m -> `Item_fault (Printf.sprintf "%s: %s" message m)
  | exn -> raise exn

let audio ~output ~faults ~log ~source_id ~format
    (stream : Stream_description.stream) =
  let conformer = Conformer.Audio.create ~log format in
  let make_chunk ~source_id ~time_base ~length frames =
    Ffmpeg_content.Audio.lift_data
      (Ffmpeg_content.Audio.Data.of_run
         (Ffmpeg_content.Audio.get_params format)
         (Ffmpeg_content.Audio.Data.run ~source_id ~time_base ~length
            (List.map (fun f -> (0, f)) frames)))
  in
  let c = chunker ~faults ~put:output.Per_stream.put ~make_chunk () in
  c.set_source source_id;
  let position = ref None in
  let outcome = ref `Ok in
  let deliver time_base frame =
    match c.push ~time_base ~position:!position frame with
      | `Ok -> ()
      | other -> outcome := other
  in
  let push ~position:p (item : Per_stream.item) =
    match item with
      | `Audio_frame frame -> (
          position := p;
          outcome := `Ok;
          try
            Conformer.Audio.convert conformer ~time_base:stream.time_base
              deliver frame;
            !outcome
          with exn -> fault "Raw audio frame dropped" exn)
      | _ -> `Item_fault "Unexpected item on a raw audio stream"
  in
  let flush () =
    (try Conformer.Audio.flush conformer deliver
     with exn ->
       log#severe "Draining raw audio failed: %s" (Printexc.to_string exn));
    c.drain ()
  in
  {
    Per_stream.sparse = false;
    push;
    switch =
      (fun ~source_id ~continuing:_ ->
        flush ();
        c.set_source source_id);
    advance = (fun _ -> ());
    flush;
  }

let video ~output ~faults ~log ~source_id ~format
    (stream : Stream_description.stream) params =
  let display = Display.create ~params ~time_base:stream.time_base () in
  let conformer = Conformer.Video.create ~log format in
  let make_chunk ~source_id ~time_base ~length frames =
    Ffmpeg_content.Video.lift_data
      (Ffmpeg_content.Video.Data.of_run
         (Ffmpeg_content.Video.get_params format)
         (Ffmpeg_content.Video.Data.run ~source_id ~time_base ~length
            (List.map (fun f -> (0, f)) frames)))
  in
  let c = chunker ~faults ~put:output.Per_stream.put ~make_chunk () in
  c.set_source source_id;
  let position = ref None in
  let outcome = ref `Ok in
  let deliver frame =
    match c.push ~time_base:stream.time_base ~position:!position frame with
      | `Ok -> ()
      | other -> outcome := other
  in
  let through_conformer frame =
    Conformer.Video.convert conformer deliver frame
  in
  let push ~position:p (item : Per_stream.item) =
    match item with
      | `Video_frame frame -> (
          position := p;
          outcome := `Ok;
          try
            Display.convert display through_conformer frame;
            !outcome
          with exn -> fault "Raw video frame dropped" exn)
      | _ -> `Item_fault "Unexpected item on a raw video stream"
  in
  let flush () =
    (try Display.eof display through_conformer
     with exn ->
       log#severe "Draining raw video failed: %s" (Printexc.to_string exn));
    c.drain ()
  in
  {
    Per_stream.sparse = false;
    push;
    switch =
      (fun ~source_id ~continuing:_ ->
        flush ();
        c.set_source source_id);
    advance = (fun _ -> ());
    flush;
  }
