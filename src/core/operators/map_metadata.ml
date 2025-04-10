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

open Source

let conf_metadata =
  Dtools.Conf.void ~p:(Configure.conf#plug "Metadata") "Metadata settings"

let conf_metadata_map =
  Dtools.Conf.void ~p:(conf_metadata#plug "map") "Metadata map settings"

let conf_metadata_map_strip =
  Dtools.Conf.bool
    ~p:(conf_metadata_map#plug "strip")
    ~d:true "Default value for the `strip` parameter."

let conf_metadata_map_insert_missing =
  Dtools.Conf.bool
    ~p:(conf_metadata_map#plug "insert_missing")
    ~d:false "Default value for the `insert_missing` parameter."

class map_metadata source rewrite_f insert_missing update strip =
  object (self)
    inherit operator ~name:"metadata.map" [source]
    initializer Typing.(self#frame_type <: Lang.unit_t)
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method seek_source = source#seek_source
    method self_sync = source#self_sync

    method private rewrite m =
      let m' = Lang.apply rewrite_f [("", Lang.metadata m)] in
      let replace_val m v =
        let x, y = Lang.to_product v in
        let x = Lang.to_string x and y = Lang.to_string y in
        if not strip then Frame.Metadata.add x y m
        else if y <> "" then Frame.Metadata.add x y m
        else Frame.Metadata.remove x m
      in
      let m = if not update then Frame.Metadata.empty else m in
      List.fold_left replace_val m (Lang.to_list m')

    method private process buf =
      List.fold_left
        (fun buf (t, m) ->
          let m = self#rewrite m in
          if strip && Frame.Metadata.is_empty m then Frame.remove_metadata buf t
          else Frame.add_metadata buf t m)
        buf
        (Frame.get_all_metadata buf)

    method private generate_frame =
      match self#split_frame source#get_frame with
        | frame, None -> self#process frame
        | frame, Some new_track ->
            let frame = self#process frame in
            let new_track = self#process new_track in
            Frame.append frame
              (match (insert_missing, Frame.get_metadata new_track 0) with
                | false, _ -> new_track
                | true, None ->
                    self#log#important "Inserting missing metadata.";
                    Frame.add_metadata new_track 0
                      (self#rewrite Frame.Metadata.empty)
                | true, Some _ -> new_track)
  end

let register =
  let return_t = Format_type.metadata in
  Lang.add_track_operator ~base:Modules.track_metadata "map"
    [
      ( "",
        Lang.fun_t
          [
            (false, "", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t));
          ]
          (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)),
        None,
        Some "A function that returns new metadata." );
      ( "update",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Update metadata. If false, existing metadata are cleared and only \
           returned values are set as new metadata." );
      ( "strip",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some
          "Completely remove empty metadata. Operates on both empty values and \
           empty metadata chunk. Defaults to `settings.metadata.map.strip` \
           when `null`." );
      ( "insert_missing",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some
          "Treat track beginnings without metadata as having empty ones. The \
           operational order is: create empty if needed, map and strip if \
           enabled. Defaults to `settings.metadata.map.insert_missing` when \
           `null`." );
      ("", return_t, None, None);
    ]
    ~category:`Track ~descr:"Rewrite metadata on the fly using a function."
    ~return_t
    (fun p ->
      let field, source = Lang.to_track (Lang.assoc "" 2 p) in
      assert (field = Frame.Fields.metadata);
      let f = Lang.assoc "" 1 p in
      let update = Lang.to_bool (List.assoc "update" p) in
      let strip = Lang.to_valued_option Lang.to_bool (List.assoc "strip" p) in
      let strip = Option.value ~default:conf_metadata_map_strip#get strip in
      let missing =
        Lang.to_valued_option Lang.to_bool (List.assoc "insert_missing" p)
      in
      let missing =
        Option.value ~default:conf_metadata_map_insert_missing#get missing
      in
      (field, new map_metadata source f missing update strip))
