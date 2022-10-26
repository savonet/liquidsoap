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

class fail ~kind name =
  object
    inherit Source.source ~name kind
    inherit Source.no_seek
    method stype = `Fallible
    method is_ready = false
    method self_sync = (`Static, false)
    method remaining = 0
    method abort_track = ()
    method get_frame _ = assert false
  end

let fail kind = (new fail ~kind "fail" :> Source.source)
let empty = fail

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "source.fail" ~category:`Input
    ~descr:
      "A source that does not produce anything. No silence, no track at all."
    ~return_t [] (fun _ ->
      (let kind = Kind.of_kind kind in
       new fail ~kind "source.fail"
        :> Source.source))

class fail_init ~kind =
  object
    inherit fail ~kind "source.fail.init"

    method wake_up _ =
      Runtime_error.raise ~pos:[] ~message:"Source's initialization failed"
        "debug"
  end

let () =
  let kind = Lang.any in
  let return_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "source.fail.init" ~category:`Input
    ~descr:
      "A source that errors during its initialization phase, used for testing \
       and debugging." ~flags:[`Experimental] ~return_t [] (fun _ ->
      (let kind = Kind.of_kind kind in
       new fail_init ~kind
        :> Source.source))
