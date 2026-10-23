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

module Data = Ffmpeg_content.Copy.Data

let packet_accessors =
  {
    Duration.get_ts =
      (function
      | `Audio p -> Avcodec.Packet.get_dts p
      | `Video p -> Avcodec.Packet.get_dts p
      | `Subtitle p -> Avcodec.Packet.get_dts p);
    set_ts =
      (fun packet ts ->
        match packet with
          | `Audio p -> Avcodec.Packet.set_dts p ts
          | `Video p -> Avcodec.Packet.set_dts p ts
          | `Subtitle p -> Avcodec.Packet.set_dts p ts);
    get_duration =
      (function
      | `Audio p -> Avcodec.Packet.get_duration p
      | `Video p -> Avcodec.Packet.get_duration p
      | `Subtitle p -> Avcodec.Packet.get_duration p);
  }

let flags : Ffmpeg_content.copy_packet -> _ = function
  | `Audio p -> Avcodec.Packet.get_flags p
  | `Video p -> Avcodec.Packet.get_flags p
  | `Subtitle p -> Avcodec.Packet.get_flags p

let is_corrupt packet = List.mem `Corrupt (flags packet)

let field_name stream =
  Frame.Fields.string_of_field stream.Stream_description.field

let create ~output ~faults ~source_id stream params =
  let source_id = ref source_id in
  let continuing = ref false in
  let time_base = stream.Stream_description.time_base in
  let filled = ref 0 in
  let put ~length items =
    if length > 0 then (
      filled := !filled + length;
      output.Per_stream.put
        (Ffmpeg_content.Copy.lift_data
           (Data.of_run (Some params)
              (Data.run ~continues:!continuing ~source_params:params
                 ~source_id:!source_id ~time_base ~length
                 (List.map (fun item -> (0, item)) items)))))
  in
  let corrupt () =
    `Item_fault
      (Printf.sprintf "Corrupt packet dropped on stream %s" (field_name stream))
  in
  match stream.media with
    | `Subtitle _ ->
        let timeline =
          Subtitle_timeline.create ~merge:( @ )
            ~delay:(fun _ packets -> packets)
            ~output:(fun ~length packets ->
              put ~length (Option.value ~default:[] packets))
            ()
        in
        let main = Time_base.main () in
        let push ~position = function
          | `Packet (`Subtitle p as packet) -> (
              let duration =
                Option.value ~default:1
                  (Option.bind
                     (Avcodec.Packet.get_duration p)
                     (Time_base.rescale_int64 ~src:time_base ~dst:main))
              in
              let data = if is_corrupt packet then [] else [packet] in
              let position =
                match position with
                  | Some _ -> position
                  | None -> Subtitle_timeline.reached timeline
              in
              match position with
                | None -> `Ok
                | Some position -> (
                    match
                      Subtitle_timeline.item timeline ~position ~duration data
                    with
                      | `Ok when data = [] -> corrupt ()
                      | `Ok ->
                          Faults.delivered faults;
                          `Ok
                      | `Item_fault _ as r -> r))
          | _ -> `Item_fault "Unexpected item on a copy subtitle stream"
        in
        {
          Per_stream.sparse = true;
          push;
          switch =
            (fun ~source_id:id ~continuing:c ->
              source_id := id;
              continuing := c);
          advance = Subtitle_timeline.advance timeline;
          flush = (fun () -> Subtitle_timeline.flush timeline);
        }
    | _ ->
        let helper = ref None in
        let get_helper ~position =
          match !helper with
            | Some h -> h
            | None ->
                let start = Option.value ~default:!filled position in
                put ~length:(start - !filled) [];
                let h =
                  Duration.create ~start ~convert_ts:false ~src:time_base
                    ~faults packet_accessors
                in
                helper := Some h;
                h
        in
        let emit = function
          | Some { Duration.length; items } -> put ~length items
          | None -> ()
        in
        let flush () =
          Option.iter (fun h -> emit (Duration.flush h)) !helper;
          helper := None
        in
        let push ~position = function
          | `Packet packet when is_corrupt packet -> corrupt ()
          | `Packet packet -> (
              match Duration.push (get_helper ~position) packet with
                | `Chunk chunk ->
                    emit (Some chunk);
                    `Ok
                | `Nothing -> `Ok
                | `Dropped -> `Ok
                | `Discontinuity reason ->
                    flush ();
                    `Source_fault reason)
          | _ -> `Item_fault "Unexpected item on a copy stream"
        in
        {
          Per_stream.sparse = false;
          push;
          switch =
            (fun ~source_id:id ~continuing:c ->
              flush ();
              source_id := id;
              continuing := c);
          advance = (fun _ -> ());
          flush;
        }
