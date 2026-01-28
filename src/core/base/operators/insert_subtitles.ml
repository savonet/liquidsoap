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

open Source

class insert_subtitles ~field (s : source option) =
  let pending = Queue.create () in
  let opt fn v = match s with Some s -> fn s | None -> v in
  object (self)
    inherit operator ~name:"track.subtitles.insert" (opt (fun s -> [s]) [])
    method fallible = opt (fun s -> s#fallible) false
    method private can_generate_frame = opt (fun s -> s#is_ready) true
    method remaining = opt (fun s -> s#remaining) (-1)
    method abort_track = opt (fun s -> s#abort_track) ()

    method effective_source =
      opt (fun s -> s#effective_source) (self :> Source.source)

    method self_sync = opt (fun s -> s#self_sync) (`Static, None)
    method insert_subtitle sub = Queue.push sub pending

    method private generate_frame =
      let buf =
        opt
          (fun s -> s#get_frame)
          (Frame.create ~length:(Lazy.force Frame.size) self#content_type)
      in
      let length = Frame.position buf in
      let existing =
        try
          let content = Frame.get buf field in
          Subtitle_content.get_data content
        with _ -> []
      in
      let inserted = Queue.fold (fun acc sub -> (0, sub) :: acc) [] pending in
      Queue.clear pending;
      let subtitles = inserted @ existing in
      let data = Subtitle_content.lift_data ~length subtitles in
      Frame.set buf field data
  end

let subtitle_insert_t =
  Lang.record_t
    [
      ("duration", Lang.float_t);
      ("text", Lang.string_t);
      ("format", Lang.string_t);
      ("forced", Lang.bool_t);
    ]

let _ =
  let frame_t = Format_type.subtitle () in
  let return_t =
    Lang.method_t frame_t
      [
        ( "insert_subtitle",
          ([], Lang.fun_t [(false, "", subtitle_insert_t)] Lang.unit_t),
          "Insert a subtitle at the current position." );
      ]
  in
  Lang.add_track_operator ~base:Modules.track_subtitles "insert"
    [("", Lang.nullable_t frame_t, Some Lang.null, None)]
    ~return_t ~category:`Track
    ~meth:
      [
        {
          name = "insert_subtitle";
          scheme = ([], Lang.fun_t [(false, "", subtitle_insert_t)] Lang.unit_t);
          descr = "Insert a subtitle at the current position.";
          value =
            (fun op ->
              Lang.val_fun
                [("", "", None)]
                (fun p ->
                  let v = List.assoc "" p in
                  let v, _ = Lang.split_meths v in
                  let duration = Lang.to_float (List.assoc "duration" v) in
                  let text = Lang.to_string (List.assoc "text" v) in
                  let format =
                    match Lang.to_string (List.assoc "format" v) with
                      | "ass" -> `Ass
                      | _ -> `Text
                  in
                  let forced = Lang.to_bool (List.assoc "forced" v) in
                  let sub : Subtitle_content.subtitle =
                    {
                      start_time = 0;
                      end_time = Frame.main_of_seconds duration;
                      text;
                      format;
                      forced;
                    }
                  in
                  op#insert_subtitle sub;
                  Lang.unit));
        };
      ]
    ~descr:
      "Allow inserting subtitles into the track. Returns the track with an \
       `insert_subtitle` method."
    (fun p ->
      let field, s =
        match Lang.to_valued_option Lang.to_track (Lang.assoc "" 1 p) with
          | None -> (Frame.Fields.subtitles, None)
          | Some (field, s) -> (field, Some s)
      in
      (field, new insert_subtitles ~field s))
