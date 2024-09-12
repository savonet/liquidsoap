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

include Liquidsoap_lang.Lang_core.MkCustom (struct
  type content = Source.source

  let name = "source.tracks"
  let to_string s = Printf.sprintf "source.tracks(source=%s)" s#id

  let to_json ~pos _ =
    Runtime_error.raise ~pos
      ~message:"Source tracks cannot be represented as json" "json"

  let compare s1 s2 = Stdlib.compare s1#id s2#id
end)

let to_value ?pos s =
  match to_value ?pos s with
    | Liquidsoap_lang.Value.Custom p ->
        Liquidsoap_lang.Value.Custom
          {
            p with
            dynamic_methods =
              Some
                {
                  hidden_methods = [];
                  methods =
                    (fun v ->
                      Some (Track.to_value (Frame.Fields.register v, s)));
                };
          }
    | _ -> assert false

let source = of_value

let fields = function
  | Liquidsoap_lang.Value.Custom { dynamic_methods = Some { hidden_methods } }
    as v
    when is_value v ->
      let source = of_value v in
      let fields =
        Frame.Fields.metadata :: Frame.Fields.track_marks
        :: List.map fst (Frame.Fields.bindings source#content_type)
      in
      List.filter
        (fun field ->
          not (List.mem (Frame.Fields.string_of_field field) hidden_methods))
        fields
  | _ -> assert false
