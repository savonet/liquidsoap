(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2019 Savonet team

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

class on_track f s =
  object
    inherit Source.operator ~name:"on_track" [s]
    method fallible = s#fallible
    method private can_generate_frame = s#is_ready
    method abort_track = s#abort_track
    method remaining = s#remaining
    method seek_source = s#seek_source
    method self_sync = s#self_sync

    method private generate_frame =
      let buf = s#get_frame in
      if Frame.track_marks buf <> [] then begin
        let m =
          match s#last_metadata with
            | None -> Lang.list []
            | Some m -> Lang.metadata m
        in
        ignore (Lang.apply f [("", m)])
      end;
      buf
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Muxer.source "on_track"
    [
      ("", Lang.source_t return_t, None, None);
      ( "",
        Lang.fun_t
          [
            (false, "", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
          ]
          Lang.unit_t,
        None,
        Some
          "Function called on every beginning of track in the stream, with the \
           corresponding metadata as argument. If there is no metadata at the \
           beginning of track, the empty list is passed. That function should \
           be fast because it is executed in the main streaming thread." );
    ]
    ~category:`Track ~descr:"Call a given handler on new tracks." ~return_t
    (fun p ->
      let s = Lang.assoc "" 1 p |> Lang.to_source in
      let f = Lang.assoc "" 2 p in
      new on_track f s)
