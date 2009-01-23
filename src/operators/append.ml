(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

open Source

class append ~insert_missing ~merge source f =
object (self)
  inherit operator [source]

  val mutable state = `Idle

  method get_frame buf =
    match state with
      | `Idle ->
          let start = Frame.position buf in
            source#get buf ;
            let finished = Frame.is_partial buf in
              begin match
                let m = Frame.get_metadata buf start in
                  if insert_missing && m = None then
                    Some (Hashtbl.create 10)
                  else
                    m
              with
                | Some m when
                  Utils.hashtbl_get m "liq_append" <> Some "false" ->
                    let append =
                      Lang.to_source
                        (Lang.apply f ["",Lang.metadata m])
                    in
                      self#register append ;
                      if finished then
                        if append#is_ready then begin
                          state <- `Append append ;
                          if merge then self#get_frame buf
                        end else begin
                          self#log#f 3
                            "Track ends and append source is not ready: \
                             won't append." ;
                          self#unregister append ;
                          state <- `Idle
                        end
                      else
                        state <- `Replay (Some append)
                | _ ->
                    self#log#f 3 "No metadata at beginning of track: \
                                  won't append." ;
                    state <- if finished then `Idle else `Replay None
              end
      | `Replay None ->
          source#get buf ;
          if Frame.is_partial buf then state <- `Idle
      | `Replay (Some a) ->
          source#get buf ;
          if Frame.is_partial buf then
            if a#is_ready then begin
              state <- `Append a ;
              if merge then self#get_frame buf
            end else begin
              self#log#f 3
                "Track ends and append source is not ready: won't append." ;
              state <- `Idle ;
              self#unregister a
            end
      | `Append a ->
          a#get buf ;
          if Frame.is_partial buf then begin
            state <- `Idle ;
            self#unregister a
          end

  method stype = source#stype

  method is_ready =
    match state with
      | `Idle | `Replay None -> source#is_ready
      | `Append s | `Replay (Some s) -> source#is_ready || s#is_ready

  method remaining =
    match state with
      | `Idle | `Replay None -> source#remaining
      | `Replay (Some s) when s#is_ready && merge ->
          let (+) a b = if a<0 || b<0 then -1 else a+b in
            source#remaining+s#remaining
      | `Replay (Some s) ->
          source#remaining
      | `Append s -> s#remaining

  (* Other behaviours could be wanted, but for now #abort_track won't cancel
   * any to-be-appended track. *)
  method abort_track = source#abort_track

  (* Finally, the administrative bit *)

  val mutable activation = []

  method wake_up activator =
    assert (state = `Idle) ;
    activation <- (self:>source)::activator ;
    source#get_ready activation ;
    Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f

  method sleep =
    source#leave (self:>source) ;
    Lang.iter_sources (fun s -> s#leave ~dynamic:true (self:>source)) f ;
    begin match state with
      | `Replay (Some a) | `Append a -> self#unregister a
      | _ -> ()
    end ;
    state <- `Idle

  method register a = a#get_ready activation
  method unregister a = a#leave (self:>source)

end

(*
 "say:$(if $(artist),\"It was $(artist)$(if $(title),\\\", $(title)\\\").\")"
 *)

let register =
  Lang.add_operator "append"
    [ "merge",Lang.bool_t,Some (Lang.bool false),
      Some "Merge the track with its appended track." ;

      "insert_missing",Lang.bool_t,Some (Lang.bool true),
      Some "Treat track beginnings without metadata as having empty one." ;

      "", Lang.source_t, None, None ;

      "",
      Lang.fun_t [false,"",Lang.metadata_t] Lang.source_t,
      None,
      Some
        "Given the metadata, build the source producing the track to append. \
         This source is allowed to fail (produce nothing) if no relevant \
         track is to be appended."
    ]
    ~category:Lang.TrackProcessing
    ~descr:"Append an extra track to every track. \
            Set the metadata 'liq_append' to 'false' to \
            inhibit appending on one track."
    (fun p ->
       let merge = Lang.to_bool (Lang.assoc "merge" 1 p) in
       let insert_missing = Lang.to_bool (Lang.assoc "insert_missing" 1 p) in
       let source = Lang.to_source (Lang.assoc "" 1 p) in
       let f = Lang.assoc "" 2 p in
         ((new append ~insert_missing ~merge source f):>source))
