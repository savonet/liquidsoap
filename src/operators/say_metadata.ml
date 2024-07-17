(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Speech synthesis of metadata *)

open Types

let from_id_assert r =
  match Request.from_id r with
  | None -> assert false
  | Some r -> r

class say_metadata source pattern =
object (self)
  inherit operator [source]

  method remaining = -1
  method stype = source#stype
  method is_ready = source#is_ready

  val mutable in_meta = false

  (* dec and req can be manipulated by different threads... *)
  val lock = Mutex.create ()
  val mutable req = None
  val mutable dec = None

  method resolve r =
    (* Resolving can take a while... *)
    let a = Request.resolve r 20. in
      (* Thus, we need to check that what we resolved is still needed... *)
      Mutex.lock lock ;
      if match req with
        | None -> false
        | Some req -> req = Request.get_id r
      then begin
        assert (dec = None) ;
        match a with
          | Request.Resolved ->
              self#log 3 "Meta-track is ready" ;
              dec <- Some
                       (match Decoder.get
                                (match Request.get_filename r with
                                   | None -> assert false
                                   | Some f -> f)
                        with
                          | None -> assert false
                          | Some d -> d) ;
              Mutex.unlock lock
          | _ -> Request.destroy r ; req <- None ; Mutex.unlock lock
      end else begin
        Mutex.unlock lock ;
        Request.destroy r
      end

  method uri meta = Utils.interpolate meta pattern

  method get_frame buf =
    if not in_meta then begin
      let start = Mixer.Buffer.position buf in
        source#get buf ;
        if Mixer.Buffer.is_partial buf then
          ( (* Add metadata synthesis at the end of track *)
            self#log 3 "Trying to append meta-track..." ;
            in_meta <- true ;
            self#get_frame buf )
        else
          if req = None then
            match Mixer.Buffer.get_metadata buf start with
            | None ->
                Mutex.lock lock ;
                req <- Some (-1) ;
                Mutex.unlock lock
            | Some m ->
                self#log 4 "First metadata seen !" ;
                match Request.create (self#uri m) with
                | Some r ->
                    Mutex.lock lock ;
                    req <- Some (Request.get_id r) ;
                    assert (dec = None) ;
                    Mutex.unlock lock ;
                    Tutils.Task.create
                      (fun () -> self#resolve r ; Tutils.Task.Finish)
                | None -> () (* no available request id *)
    end else
      match dec with
      | Some d ->
          ignore (d.Decoder.fill buf) ;
          if Mixer.Buffer.is_partial buf then
            ( self#log 4 "Meta-track finished." ;
              d.Decoder.close () ;
              dec <- None ;
              Request.destroy
                (match req with
                 | Some r -> (from_id_assert r)
                 | None -> assert false) ;
              req <- None ;
              in_meta <- false )
      | None -> self#log 4 "No meta track." ; self#reset_meta

  method reset_meta =
    in_meta <- false ;
    Mutex.lock lock ;
    match req with
      | Some r ->
	  ( match dec with
	      | Some d ->
                  d.Decoder.close () ;
                  dec <- None ;
                  Request.destroy (from_id_assert r)
	      | None -> () ) ;
	  req <- None ;
	  Mutex.unlock lock
      | None -> assert (dec = None) ; Mutex.unlock lock

  method abort_track =
    self#reset_meta ;
    source#abort_track

end

let default_pattern =
  "say:$(if $(artist),\"It was $(artist)$(if $(title),\\\", $(title)\\\").\")"

let register =
  Lang.add_operator "say_metadata"
    [ "", Lang.source_t, None, None ;
      "pattern", Lang.string_t, Some (Lang.string default_pattern), None ]
    ~descr:"Pronounce some metadata after a track."
    (fun p ->
       let f v = List.assoc v p in
       let source = Lang.to_source (f "") in
       let pattern = Lang.to_string (f "pattern") in
         ((new say_metadata source pattern):>source))
