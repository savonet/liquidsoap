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

let rectangle_subtitle ~main ~milli (content : Avutil.Subtitle.content)
    (r : Avutil.Subtitle.rectangle) =
  let to_main ms =
    Option.value ~default:0 (Time_base.rescale ~src:milli ~dst:main ms)
  in
  let subtitle text format =
    Some
      {
        Subtitle_content.start_time = to_main content.start_display_time;
        end_time = to_main content.end_display_time;
        text;
        format;
        forced = List.mem `Forced r.flags;
      }
  in
  match r.rect_type with
    | `Ass -> subtitle r.ass `Ass
    | `Text -> subtitle r.text `Text
    | `Bitmap | `None -> None

let delay d (subtitles : Subtitle_content.subtitle list) =
  List.map
    (fun (s : Subtitle_content.subtitle) ->
      {
        s with
        start_time = max 0 (s.start_time - d);
        end_time = max 0 (s.end_time - d);
      })
    subtitles

let create ~output ~faults ~field =
  let kind = Frame.Fields.string_of_field field in
  let main = Time_base.main () and milli = { Avutil.num = 1; den = 1000 } in
  let timeline =
    Subtitle_timeline.create ~merge:( @ ) ~delay
      ~output:(fun ~length subtitles ->
        output.Per_stream.put
          (Subtitle_content.lift_data ~length
             (List.map (fun s -> (0, s)) (Option.value ~default:[] subtitles))))
      ()
  in
  let push ~position (item : Per_stream.item) =
    match item with
      | `Subtitle_frame frame -> (
          let content = Avutil.Subtitle.get_content frame in
          let subtitles =
            List.filter_map
              (rectangle_subtitle ~main ~milli content)
              content.rectangles
          in
          let skipped =
            List.length content.rectangles - List.length subtitles
          in
          let position =
            match position with
              | Some _ -> position
              | None -> Subtitle_timeline.reached timeline
          in
          match (subtitles, position) with
            | [], _ when skipped > 0 ->
                `Item_fault
                  (Printf.sprintf "Subtitle on %s dropped: no text rectangle"
                     kind)
            | _, None | [], _ -> `Ok
            | _, Some position -> (
                if skipped > 0 then
                  Faults.uncounted faults ~kind
                    (Printf.sprintf "%d non-text subtitle rectangles skipped"
                       skipped);
                let duration =
                  List.fold_left
                    (fun d (s : Subtitle_content.subtitle) -> max d s.end_time)
                    0 subtitles
                in
                match
                  Subtitle_timeline.item timeline ~position ~duration subtitles
                with
                  | `Ok ->
                      Faults.delivered faults;
                      `Ok
                  | `Item_fault _ as fault -> fault))
      | _ -> `Item_fault "Unexpected item on a subtitle stream"
  in
  {
    Per_stream.sparse = true;
    push;
    switch = (fun ~source_id:_ ~continuing:_ -> ());
    advance = Subtitle_timeline.advance timeline;
    flush = (fun () -> Subtitle_timeline.flush timeline);
  }
