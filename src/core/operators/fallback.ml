(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

class fallback ~track_sensitive sources =
  let self_sync_type = Utils.self_sync_type sources in
  object (self)
    inherit Source.operator ~name:"fallback" sources
    val mutable selected = None
    method selected = selected

    method stype =
      if List.exists (fun s -> s#stype = `Infallible) sources then `Infallible
      else `Fallible

    method self_sync =
      ( Lazy.force self_sync_type,
        match selected with Some s -> snd s#self_sync | None -> false )

    method remaining =
      match selected with
        | Some s when track_sensitive () -> s#remaining
        | _ -> -1

    method seek n = match selected with Some s -> s#seek n | None -> 0
    method is_ready = List.exists (fun s -> s#is_ready) sources

    method private get_frame buf =
      (* This is guanranteed to succeed because if #is_ready above *)
      let s =
        match selected with
          | Some s when track_sensitive () && s#is_ready -> s
          | _ ->
              let s = List.find (fun s -> s#is_ready) sources in
              self#log#info "selected %s" s#id;
              selected <- Some s;
              s
      in
      s#get buf

    method abort_track =
      match selected with Some s -> s#abort_track | None -> ()
  end

let () =
  let kind = Lang.any in
  let return_t = Lang.frame_kind_t kind in
  Lang.add_operator "fallback" ~return_t
    ~meth:
      [
        ( "selected",
          ([], Lang.fun_t [] (Lang.nullable_t (Lang.source_t return_t))),
          "Currently selected source.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#selected with
                  | Some s -> Lang.source s
                  | None -> Lang.null) );
      ]
    ~category:`Track
    ~descr:"Select source to play based on first source available."
    [
      ( "track_sensitive",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool false),
        Some "Re-select only on end of tracks." );
      ("", Lang.list_t (Lang.source_t return_t), None, None);
    ]
    (fun p ->
      let track_sensitive = Lang.to_getter (List.assoc "track_sensitive" p) in
      let track_sensitive () = Lang.to_bool (track_sensitive ()) in
      let sources = List.map Lang.to_source (Lang.to_list (List.assoc "" p)) in
      new fallback ~track_sensitive sources)
