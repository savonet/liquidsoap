(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

type field = {
  target_field : Frame.field;
  source_field : Frame.field;
  processor : Content.data -> Content.data;
}

type track = { mutable fields : field list; source : Source.source }

class muxer tracks =
  let sources =
    List.fold_left
      (fun sources { source } ->
        if List.memq source sources then sources else source :: sources)
      [] tracks
  in
  let stype =
    if List.for_all (fun s -> s#stype = `Infallible) sources then `Infallible
    else `Fallible
  in
  let self_sync_type = Utils.self_sync_type sources in
  let track_frames = ref [] in
  let clear_track_frames () =
    List.iter (fun (_, frame) -> Frame.clear frame) !track_frames
  in
  object (self)
    inherit Source.operator ~name:"source" sources
    method stype = stype

    method self_sync =
      (Lazy.force self_sync_type, List.exists (fun s -> snd s#self_sync) sources)

    method abort_track = List.iter (fun s -> s#abort_track) sources
    method private sources_ready = List.for_all (fun s -> s#is_ready) sources
    method is_ready = Generator.length self#buffer > 0 || self#sources_ready

    method seek len =
      let len = min (Generator.length self#buffer) len in
      Generator.truncate self#buffer len;
      len

    method remaining =
      match
        ( Generator.remaining self#buffer,
          List.fold_left
            (fun r s -> if r = -1 then s#remaining else min r s#remaining)
            (-1)
            (List.filter (fun s -> s#is_ready) sources) )
      with
        | -1, r -> r
        | r, _ -> r

    method private track_frame source =
      try List.assq source !track_frames
      with Not_found ->
        let f = Frame.create source#content_type in
        track_frames := (source, f) :: !track_frames;
        f

    method private feed_track ~tmp ~filled ~start ~stop
        { source_field; target_field; processor } =
      let c = Content.sub (Frame.get tmp source_field) start (stop - start) in
      match source_field with
        | f when f = Frame.Fields.metadata ->
            List.iter
              (fun (pos, m) -> Generator.add_metadata ~pos self#buffer m)
              (Content.Metadata.get_data c)
        | f when f = Frame.Fields.track_marks ->
            if Frame.is_partial tmp then
              Generator.add_track_mark ~pos:(stop - filled) self#buffer
        | _ -> Generator.put self#buffer target_field (processor c)

    method private feed_fields ~filled { fields; source } =
      let tmp = self#track_frame source in
      let start = Frame.position tmp in
      if Frame.is_partial tmp && source#is_ready then (
        source#get tmp;
        let stop = Frame.position tmp in
        List.iter (self#feed_track ~tmp ~filled ~start ~stop) fields)

    (* In the current streaming model, we might still need to force
       a source#get to get the next break/metadata at the beginning of
       a track.

       TODO: get rid of this! *)
    method private feed ~force buf =
      let filled = Frame.position buf in
      if
        self#sources_ready
        && (force
           || Generator.remaining self#buffer = -1
              && filled + Generator.length self#buffer < Lazy.force Frame.size)
      then (
        List.iter (self#feed_fields ~filled) tracks;
        self#feed ~force:false buf)

    (* There are two situations here:
       - If we are tracking track marks from a source,
         we do need to stop at each track mark and let
         consumers of the muxed source call us back.
       - If we are not tracking track marks, we actually
         need to recurse when one of the source has one.

       Therefore, the implementation:
       - Keeps a global buffer of generated data.
       - Keeps a temporary track for each pulled sources
       - Implements the recursion above
       - Clears all temporary buffer at the end of each
         streaming cycle.

       This means that we do not buffer beyond the current
       frame and, also, that if one source becomes unavailable
       while streaming, we end all tracks when this source drops
       off. *)
    method get_frame buf =
      self#feed ~force:true buf;
      Generator.fill self#buffer buf

    initializer
      self#on_after_output (fun () ->
          clear_track_frames ();
          Frame.clear self#buffer)
  end

let muxer_operator p =
  let tracks = List.assoc "" p in
  let processor c = c in
  let tracks =
    List.fold_left
      (fun tracks (label, t) ->
        let source_field, s = Lang.to_track t in
        let target_field = Frame.Fields.register label in
        let field = { source_field; target_field; processor } in
        match List.find_opt (fun { source } -> source == s) tracks with
          | Some track ->
              track.fields <- field :: track.fields;
              tracks
          | None -> { source = s; fields = [field] } :: tracks)
      []
      (fst (Lang.split_meths tracks))
  in
  if
    List.for_all
      (fun { fields } ->
        List.for_all
          (fun { target_field } ->
            target_field = Frame.Fields.metadata
            || target_field = Frame.Fields.track_marks)
          fields)
      tracks
  then
    Runtime_error.raise ~pos:(Lang.pos p)
      ~message:
        "source muxer needs at least one track with content that is not \
         metadata or track_marks!"
      "invalid";
  let s = new muxer tracks in
  let target_fields =
    List.fold_left
      (fun target_fields { source; fields } ->
        let source_fields, target_fields =
          List.fold_left
            (fun (source_fields, target_fields) { source_field; target_field } ->
              match source_field with
                | f when f = Frame.Fields.metadata ->
                    (source_fields, target_fields)
                | f when f = Frame.Fields.track_marks ->
                    (source_fields, target_fields)
                | _ ->
                    let source_field_t = Lang.univ_t () in
                    ( Frame.Fields.add source_field source_field_t source_fields,
                      Frame.Fields.add target_field source_field_t target_fields
                    ))
            (Frame.Fields.empty, target_fields)
            fields
        in
        let source_frame_t = Lang.frame_t (Lang.univ_t ()) source_fields in
        Typing.(source#frame_type <: source_frame_t);
        target_fields)
      Frame.Fields.empty tracks
  in
  Typing.(s#frame_type <: Lang.frame_t (Lang.univ_t ()) target_fields);
  s

let source =
  let frame_t = Lang.univ_t ~constraints:[Format_type.muxed_tracks] () in
  let tracks_t =
    Type.meth ~optional:true "track_marks"
      ([], Format_type.track_marks)
      (Type.meth ~optional:true "metadata" ([], Format_type.metadata) frame_t)
  in
  Lang.add_operator "source" ~category:`Input
    ~descr:"Create a source that muxes the given tracks." ~return_t:frame_t
    [("", tracks_t, None, Some "Tracks to mux")]
    muxer_operator

let _ =
  let track_t = Lang.univ_t ~constraints:[Format_type.track] () in
  let return_t = Format_type.track_marks in
  Lang.add_track_operator ~base:Modules.track "track_marks" ~category:`Track
    ~descr:"Return the track marks associated with the given track" ~return_t
    [("", track_t, None, None)]
    (fun p ->
      let _, s = Lang.to_track (List.assoc "" p) in
      (Frame.Fields.track_marks, s))

let track_metadata =
  let track_t = Lang.univ_t ~constraints:[Format_type.track] () in
  let return_t = Format_type.metadata in
  Lang.add_track_operator ~base:Modules.track "metadata" ~category:`Track
    ~descr:"Return the metadata associated with the given track" ~return_t
    [("", track_t, None, None)]
    (fun p ->
      let _, s = Lang.to_track (List.assoc "" p) in
      (Frame.Fields.metadata, s))

let _ =
  let frame_t = Lang.univ_t () in
  let return_t = Lang_source.source_tracks_t frame_t in
  let arguments = [("", Lang.source_t ~methods:false frame_t, None, None)] in
  Lang.add_builtin ~base:source "tracks" ~category:(`Source `Track)
    ~descr:"Return the tracks of a given source." arguments return_t (fun env ->
      let return_t = Lang_source.check_arguments ~return_t ~env arguments in
      let return_t =
        Type.filter_meths return_t (fun { Type.meth } ->
            meth <> "metadata" && meth <> "track_marks")
      in
      let s = Lang.to_source (List.assoc "" env) in
      Typing.(s#frame_type <: return_t);
      Lang_source.source_tracks s)
