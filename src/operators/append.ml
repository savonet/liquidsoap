(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

open Source

class append ~kind ~insert_missing ~merge source f =
  object (self)
    inherit operator ~name:"append" kind [source]

    val mutable state = `Idle

    method private get_frame buf =
      match state with
        | `Idle -> (
            let start = Frame.position buf in
            source#get buf;
            let finished = Frame.is_partial buf in
            match
              let m = Frame.get_metadata buf start in
              if insert_missing && m = None then Some (Hashtbl.create 10) else m
            with
              | Some m when Utils.hashtbl_get m "liq_append" <> Some "false" ->
                  let t = Lang.source_t (Lang.kind_type_of_frame_kind kind) in
                  let append =
                    Lang.to_source (Lang.apply ~t f [("", Lang.metadata m)])
                  in
                  self#register append;
                  if finished then
                    if append#is_ready then (
                      state <- `Append append;
                      if merge then (
                        let pos = Frame.position buf in
                        self#get_frame buf;
                        Frame.set_breaks buf
                          (Utils.remove_one (( = ) pos) (Frame.breaks buf)) ) )
                    else (
                      self#log#important
                        "Track ends and append source is not ready: won't \
                         append.";
                      self#unregister append;
                      state <- `Idle )
                  else state <- `Replay (Some append)
              | _ ->
                  self#log#important
                    "No metadata at beginning of track: won't append.";
                  state <- (if finished then `Idle else `Replay None) )
        | `Replay None ->
            source#get buf;
            if Frame.is_partial buf then state <- `Idle
        | `Replay (Some a) ->
            source#get buf;
            if Frame.is_partial buf then
              if a#is_ready then (
                state <- `Append a;
                if merge then (
                  let pos = Frame.position buf in
                  self#get_frame buf;
                  Frame.set_breaks buf
                    (Utils.remove_one (( = ) pos) (Frame.breaks buf)) ) )
              else (
                self#log#important
                  "Track ends and append source is not ready: won't append.";
                state <- `Idle;
                self#unregister a )
        | `Append a ->
            a#get buf;
            if Frame.is_partial buf then (
              state <- `Idle;
              self#unregister a )

    method stype = source#stype

    method is_ready =
      match state with
        | `Idle | `Replay None -> source#is_ready
        | `Append s | `Replay (Some s) -> source#is_ready || s#is_ready

    method remaining =
      match state with
        | `Idle | `Replay None -> source#remaining
        | `Replay (Some s) when s#is_ready && merge ->
            let ( + ) a b = if a < 0 || b < 0 then -1 else a + b in
            source#remaining + s#remaining
        | `Replay (Some _) -> source#remaining
        | `Append s -> s#remaining

    method seek n =
      match state with
        | `Idle | `Replay None -> source#seek n
        | `Replay (Some s) when s#is_ready && merge -> 0
        | `Replay (Some _) -> source#seek n
        | `Append s -> s#seek n

    method self_sync =
      match state with `Append s -> s#self_sync | _ -> source#self_sync

    (* Other behaviours could be wanted, but for now #abort_track won't cancel
     * any to-be-appended track. *)
    method abort_track = source#abort_track

    (* Finally, the administrative bit *)
    val mutable activation = []

    method private wake_up activator =
      assert (state = `Idle);
      activation <- (self :> source) :: activator;
      source#get_ready activation;
      Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f

    method private sleep =
      source#leave (self :> source);
      Lang.iter_sources (fun s -> s#leave ~dynamic:true (self :> source)) f;
      begin
        match state with
        | `Replay (Some a) | `Append a -> self#unregister a
        | _ -> ()
      end;
      state <- `Idle

    method private register a = a#get_ready activation

    method private unregister a = a#leave (self :> source)
  end

let register =
  let k = Lang.univ_t () in
  Lang.add_operator "append"
    [
      ( "merge",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Merge the track with its appended track." );
      ( "insert_missing",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Treat track beginnings without metadata as having empty one." );
      ("", Lang.source_t k, None, None);
      ( "",
        Lang.fun_t [(false, "", Lang.metadata_t)] (Lang.source_t k),
        None,
        Some
          "Given the metadata, build the source producing the track to append. \
           This source is allowed to fail (produce nothing) if no relevant \
           track is to be appended." );
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.TrackProcessing
    ~descr:
      "Append an extra track to every track. Set the metadata 'liq_append' to \
       'false' to inhibit appending on one track."
    (fun p kind ->
      let merge = Lang.to_bool (Lang.assoc "merge" 1 p) in
      let insert_missing = Lang.to_bool (Lang.assoc "insert_missing" 1 p) in
      let source = Lang.to_source (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      new append ~kind ~insert_missing ~merge source f)
