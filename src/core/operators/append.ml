(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

class append ~insert_missing ~merge source f =
  let sources = ref [] in
  let failed = ref false in
  let () =
    Lang.iter_sources
      ~on_imprecise:(fun () -> failed := true)
      (fun s -> sources := s :: !sources)
      f
  in
  let self_sync_type =
    if !failed then lazy `Dynamic else Utils.self_sync_type !sources
  in
  let state = Atomic.make `Idle in
  object (self)
    inherit operator ~name:"append" [source]
    val mutable last_append_metadata = None

    method private get_frame buf =
      match Atomic.get state with
        | `Idle ->
            source#get buf;
            if Frame.is_partial buf then (
              let last_metadata =
                if last_append_metadata == self#last_metadata then None
                else self#last_metadata
              in
              last_append_metadata <- last_metadata;
              match
                if insert_missing && last_metadata = None then
                  Some (Hashtbl.create 10)
                else last_metadata
              with
                | Some m ->
                    let append =
                      Lang.to_source (Lang.apply f [("", Lang.metadata m)])
                    in
                    self#register append;
                    Atomic.set state (`Append append);
                    if merge then
                      if not append#is_ready then (
                        self#log#important
                          "Track ends and append source is not ready: won't \
                           append.";
                        self#unregister append;
                        Atomic.set state `Idle)
                      else (
                        let pos = Frame.position buf in
                        self#get_frame buf;
                        Frame.set_breaks buf
                          (Utils.remove_one (( = ) pos) (Frame.breaks buf)))
                | None ->
                    self#log#important
                      "No metadata at beginning of track: won't append.")
        | `Append a ->
            a#get buf;
            if Frame.is_partial buf then (
              Atomic.set state `Idle;
              self#unregister a)

    method stype = source#stype

    method private seek_source =
      match Atomic.get state with
        | `Append s -> s#seek_source
        | `Idle -> source#seek_source

    method is_ready =
      match (Atomic.get state, self#stype) with
        | `Append a, `Infallible when not a#is_ready ->
            self#unregister a;
            Atomic.set state `Idle;
            true
        | `Append a, _ -> a#is_ready
        | `Idle, _ -> source#is_ready

    method remaining = self#seek_source#remaining
    method seek = self#seek_source#seek

    method self_sync =
      (Lazy.force self_sync_type, snd self#seek_source#self_sync)

    method abort_track = self#seek_source#abort_track

    (* Finally, the administrative bit *)
    val mutable activation = []

    method! private wake_up activator =
      assert (Atomic.get state = `Idle);
      activation <- (self :> source) :: activator;
      source#get_ready activation;
      Lang.iter_sources (fun s -> s#get_ready ~dynamic:true activation) f

    method! private sleep =
      source#leave (self :> source);
      Lang.iter_sources (fun s -> s#leave ~dynamic:true (self :> source)) f;
      begin
        match Atomic.get state with `Append a -> self#unregister a | _ -> ()
      end;
      Atomic.set state `Idle

    method private register a =
      Clock.unify a#clock source#clock;
      Typing.(a#frame_type <: self#frame_type);
      a#get_ready activation

    method private unregister a = a#leave (self :> source)
  end

let _ =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
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
      ("", Lang.source_t frame_t, None, None);
      ( "",
        Lang.fun_t [(false, "", Lang.metadata_t)] (Lang.source_t frame_t),
        None,
        Some
          "Given the metadata, build the source producing the track to append. \
           This source is allowed to fail (produce nothing) if no relevant \
           track is to be appended." );
    ]
    ~return_t:frame_t ~category:`Track
    ~descr:
      "Append an extra track to every track. Set the metadata 'liq_append' to \
       'false' to inhibit appending on one track."
    (fun p ->
      let merge = Lang.to_bool (Lang.assoc "merge" 1 p) in
      let insert_missing = Lang.to_bool (Lang.assoc "insert_missing" 1 p) in
      let source = Lang.to_source (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      new append ~insert_missing ~merge source f)
