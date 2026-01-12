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

(** This one is a bit tricky as we want to make sure that the underlying source
    is cleaned up when it's done pulling. Used in switch-based transitions to
    avoid infinite stack of sources. *)
class max_duration ~override_meta ~duration source =
  object (self)
    inherit Source.operator ~name:"max_duration" [source]
    initializer Clock.unify ~pos:self#pos self#clock source#clock
    val mutable remaining = duration
    val mutable s = source
    method self_sync = source#self_sync
    method fallible = true
    method private can_generate_frame = remaining > 0 && s#is_ready
    method abort_track = s#abort_track

    method remaining =
      match (remaining, s#remaining) with
        | 0, _ -> 0
        | _, -1 -> -1
        | rem, rem' -> min rem rem'

    method! seek len = source#effective_source#seek (min remaining len)
    method effective_source = source#effective_source

    method private check_for_override buf =
      List.iter
        (fun (_, m) ->
          Frame.Metadata.iter
            (fun lbl v ->
              if lbl = override_meta then (
                try
                  let v = float_of_string v in
                  remaining <- Frame.main_of_seconds v;
                  self#log#info "Overriding remaining value: %.02f." v
                with _ ->
                  self#log#important "Invalid remaining override value: %s." v))
            m)
        (Frame.get_all_metadata buf)

    method private generate_frame =
      let buf = s#get_frame in
      self#check_for_override buf;
      let pos = Frame.position buf in
      let len = min remaining pos in
      remaining <- remaining - len;
      if remaining <= 0 then s <- Debug_sources.empty ();
      if len < pos then Frame.slice buf len else buf
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator "max_duration"
    [
      ( "override",
        Lang.string_t,
        Some (Lang.string "liq_remaining"),
        Some
          "Metadata field which, if present and containing a float, overrides \
           the remaining play time." );
      ("", Lang.float_t, None, Some "Maximum duration");
      ("", Lang.source_t return_t, None, None);
    ]
    ~category:`Track ~descr:"Limit source duration" ~return_t
    (fun p ->
      let override_meta = Lang.to_string (List.assoc "override" p) in
      let duration =
        Frame.main_of_seconds (Lang.to_float (Lang.assoc "" 1 p))
      in
      let s = Lang.to_source (Lang.assoc "" 2 p) in
      new max_duration ~override_meta ~duration s)
