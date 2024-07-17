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

open Source

let hashtbl_to_langlist h =
  let l = Hashtbl.fold (fun k v l -> (k,v)::l) h [] in
    Lang.list
      (List.map (fun (a,b) -> Lang.product (Lang.string a) (Lang.string b)) l)

class prepend ~merge source f =
object (self)
  inherit operator [source]

  val mutable state = `Idle

  val start = Mixer.Buffer.size - 4

  method get_frame buf =
    match state with
      | `Idle ->
          let peek =
            let b = Mixer.Buffer.create () in
              Mixer.Buffer.add_break b start ;
              b
          in
            source#get peek ;
            if Mixer.Buffer.position peek = start then
              Mixer.Buffer.add_break buf (Mixer.Buffer.position buf)
            else begin
              match Mixer.Buffer.get_metadata peek start with
                | Some m when
                  Utils.hashtbl_get m "liq_prepend" <> Some "false" ->
                    let prepend =
                      Lang.to_source
                        (Lang.apply f ["",hashtbl_to_langlist m])
                    in
                      self#register prepend ;
                      if not prepend#is_ready then begin
                        self#log 2
                          "Candidate to prepending not ready. Abort!" ;
                        state <- `Buffer peek ;
                        self#unregister prepend
                      end else begin
                        state <- `Prepend (prepend,peek)
                      end ;
                      self#get_frame buf
                | _ ->
                    state <- `Buffer peek ;
                    self#get_frame buf
            end
      | `Buffer peek ->
          let p = Mixer.Buffer.position buf in
            for i=0 to 3 do
              (Mixer.Buffer.to_string buf).[p+i] <-
              (Mixer.Buffer.to_string peek).[start+i]
            done ;
            Mixer.Buffer.add_break buf (p+4) ;
            begin match Mixer.Buffer.get_metadata peek start with
              | Some m -> Mixer.Buffer.set_metadata buf p m
              | None -> ()
            end ;
            state <- `Replay ;
            self#get_frame buf
      | `Replay ->
          source#get buf ;
          if Mixer.Buffer.is_partial buf then state <- `Idle
      | `Prepend (prepend,peek) ->
          prepend#get buf ;
          if Mixer.Buffer.is_partial buf then begin
            self#unregister prepend ;
            state <- `Buffer peek ;
            if merge then self#get_frame buf
          end

  method stype = source#stype

  method is_ready =
    match state with
      | `Idle | `Replay | `Buffer _ -> source#is_ready
      | `Prepend (s,_) -> s#is_ready || source#is_ready

  method remaining =
    match state with
      | `Idle | `Replay | `Buffer _ -> source#remaining
      | `Prepend (s,_) ->
          let (+) a b = if a<0 || b<0 then -1 else a+b in
            if merge then s#remaining+source#remaining else s#remaining

  (* Other behaviours could be wanted, but for now #abort_track won't abort
   * the prepended track. *)
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
      | `Prepend (p,_) -> self#unregister p
      | _ -> ()
    end ;
    state <- `Idle

  method register a = a#get_ready activation
  method unregister a = a#leave (self:>source)

end

let register =
  Lang.add_operator "prepend"
    [ "merge",Lang.bool_t,Some (Lang.bool false),
      Some "Merge the track with its appended track." ;
      
      "", Lang.source_t, None, None ;

      "",
      Lang.fun_t
        [false,"",Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)]
        Lang.source_t,
      None,
      Some
        ("Given the metadata, build the source producing the track to prepend."^
         " This source is allowed to fail (produce nothing) if no relevant "^
         "track is to be appended. However, success must be immediate.")
    ]
    ~descr:("Prepend an extra track before every track. "^
            "Set the metadata 'liq_prepend' to 'false' to "^
            "inhibit prepending on one track.")
    (fun p ->
       let merge = Lang.to_bool (Lang.assoc "merge" 1 p) in
       let source = Lang.to_source (Lang.assoc "" 1 p) in
       let f = Lang.assoc "" 2 p in
         ((new prepend ~merge source f):>source))
