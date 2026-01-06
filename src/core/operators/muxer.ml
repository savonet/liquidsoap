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

type field = { target_field : Frame.field; source_field : Frame.field }
type track = { mutable fields : field list; source : Source.source }

class muxer ~pos ~base tracks =
  let sources =
    List.fold_left
      (fun sources { source } ->
        if List.memq source sources then sources else source :: sources)
      (match base with Some s -> [Source_tracks.source s] | None -> [])
      tracks
  in
  let fallible = List.exists (fun s -> s#fallible) sources in
  let self_sync = Clock_base.self_sync sources in
  object (self)
    (* Pass duplicated list to operator to make sure caching is properly enabled. *)
    inherit Source.operator ~name:"source" sources
    method self_sync = self_sync ~source:self ()
    method fallible = fallible
    method abort_track = List.iter (fun s -> s#abort_track) sources
    method private sources_ready = List.for_all (fun s -> s#is_ready) sources
    method private can_generate_frame = self#sources_ready

    method! seek len =
      let s = self#effective_source in
      if s == (self :> Source.source) then (
        self#log#info
          "Operator is muxing from multiple sources and cannot seek without \
           risking losing content synchronization!";
        len)
      else s#seek len

    method effective_source =
      match
        List.fold_left
          (fun sources source ->
            let source = source#effective_source in
            if List.memq source sources then sources else source :: sources)
          [] sources
      with
        | [s] -> s#effective_source
        | _ -> (self :> Source.source)

    method remaining =
      List.fold_left
        (fun r s -> if r = -1 then s#remaining else min r s#remaining)
        (-1)
        (List.filter (fun (s : Source.source) -> s#is_ready) sources)

    val mutable muxed_tracks = None

    method private tracks =
      match muxed_tracks with
        | Some s -> s
        | None ->
            let base =
              match base with
                | Some source_tracks ->
                    let fields =
                      List.map
                        (fun source_field ->
                          { source_field; target_field = source_field })
                        (Source_tracks.fields source_tracks)
                    in
                    [{ source = Source_tracks.source source_tracks; fields }]
                | None -> []
            in
            let tracks =
              match
                ( base,
                  List.partition
                    (fun { source = s } ->
                      List.exists (fun { source = s' } -> s == s') base)
                    tracks )
              with
                | _, ([], _) -> base @ tracks
                | [{ fields = f }], ([({ fields = f' } as p)], tracks) ->
                    {
                      p with
                      fields =
                        f'
                        @ List.filter
                            (fun { target_field = t } ->
                              List.exists
                                (fun { target_field = t' } -> t = t')
                                f')
                            f;
                    }
                    :: tracks
                | _ -> assert false
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
              Runtime_error.raise ~pos
                ~message:
                  "source muxer needs at least one track with content that is \
                   not metadata or track_marks!"
                "invalid";
            muxed_tracks <- Some tracks;
            tracks

    method generate_frame =
      let length = Lazy.force Frame.size in
      let pos, frame =
        List.fold_left
          (fun (pos, frame) { fields; source } ->
            let buf = source#get_frame in
            ( min pos (Frame.position buf),
              List.fold_left
                (fun frame { source_field; target_field } ->
                  let c = Frame.get buf source_field in
                  Frame.set frame target_field c)
                frame fields ))
          (length, Frame.create ~length Frame.Fields.empty)
          self#tracks
      in
      Frame.slice frame pos
  end

let muxer_operator p =
  let base, tracks =
    match List.assoc "" p with
      | Liquidsoap_lang.Value.Custom { methods } as v
        when Source_tracks.is_value v ->
          (Some v, methods)
      | v -> (None, Liquidsoap_lang.Value.methods v)
  in
  let tracks =
    List.fold_left
      (fun tracks (label, t) ->
        let source_field, s = Lang.to_track t in
        let target_field = Frame.Fields.register label in
        let field = { source_field; target_field } in
        match List.find_opt (fun { source } -> source == s) tracks with
          | Some track ->
              track.fields <- field :: track.fields;
              tracks
          | None -> { source = s; fields = [field] } :: tracks)
      []
      (Liquidsoap_lang.Methods.bindings tracks)
  in
  let s = new muxer ~pos:(try Lang.pos p with _ -> []) ~base tracks in
  let target_fields =
    List.fold_left
      (fun target_fields { source; fields } ->
        let source_fields, target_fields =
          List.fold_left
            (fun (source_fields, target_fields) { source_field; target_field }
               ->
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
  Typing.(
    s#frame_type
    <: Lang.frame_t
         (match base with
           | Some s -> (Source_tracks.source s)#frame_type
           | None -> Lang.univ_t ())
         target_fields);
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
      let return_t, env =
        Lang_source.check_arguments ~return_t ~env arguments
      in
      let return_t =
        Type.filter_meths return_t (fun { Type.name = meth } ->
            meth <> "metadata" && meth <> "track_marks")
      in
      let source_val = List.assoc "" env in
      let s = Lang.to_source source_val in
      Typing.(s#frame_type <: return_t);
      Source_tracks.to_value s)
