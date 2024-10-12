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

class fail name =
  object (self)
    inherit Source.source ~name ()
    method seek_source = (self :> Source.source)
    method fallible = true
    method private can_generate_frame = false
    method self_sync = (`Static, None)
    method remaining = 0
    method abort_track = ()
    method generate_frame = assert false
  end

let fail () = (new fail "fail" :> Source.source)
let empty = fail

let fail =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Muxer.source "fail" ~category:`Input
    ~descr:
      "A source that does not produce anything. No silence, no track at all."
    ~return_t [] (fun _ -> (new fail "source.fail" :> Source.source))

class fail_init =
  object (self)
    inherit fail "source.fail.init"

    initializer
      self#on_wake_up (fun () ->
          Lang.raise_error ~pos:[] ~message:"Source's initialization failed"
            "debug")
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:fail "init" ~category:`Input
    ~descr:
      "A source that errors during its initialization phase, used for testing \
       and debugging." ~flags:[`Experimental] ~return_t [] (fun _ ->
      new fail_init)

class is_ready s =
  object (self)
    inherit Source.operator ~name:"is_ready" [s]
    method seek_source = (self :> Source.source)
    method fallible = true
    method private can_generate_frame = true
    method self_sync = (`Static, None)
    method remaining = 0
    method abort_track = ()
    method generate_frame = if s#is_ready then s#get_frame else self#empty_frame
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Modules.debug "is_ready" ~category:`Input
    ~descr:
      "A source that always produces an empty frame when the underlying source \
       is not ready, used for testing and debugging."
    ~flags:[`Experimental] ~return_t
    [("", Lang.source_t return_t, None, None)]
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      new is_ready s)
