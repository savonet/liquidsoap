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

class prepend ~kind ~merge source f =
  object (self)
    inherit operator ~name:"prepend" kind [source]

    val mutable state = `Idle

    method private get_frame buf =
      match state with
        | `Idle ->
            (* We're at the beginning of a track.
             * Let's peek one sample of data and read its metadata. *)
            (* TODO how does that play with caching ? *)
            let peek = Frame.create kind in
            let peekpos = AFrame.size () - 1 in
            AFrame.add_break peek peekpos;
            source#get peek;
            if AFrame.is_partial peek then
              AFrame.add_break buf (AFrame.position buf)
            else (
              let inhibit, lang_m =
                match AFrame.get_metadata peek peekpos with
                  | Some m ->
                      ( Utils.hashtbl_get m "liq_prepend" = Some "false",
                        Lang.metadata m )
                  | None ->
                      ( false,
                        Lang.list
                          ~t:(Lang.product_t Lang.string_t Lang.string_t)
                          [] )
              in
              if inhibit then (
                self#log#info
                  "Prepending disabled from metadata \
                   (\"liq_prepend\"=\"false\").";
                state <- `Buffer peek;
                self#get_frame buf )
              else (
                let t = Lang.source_t (Lang.kind_type_of_frame_kind kind) in
                let prepend = Lang.to_source (Lang.apply ~t f [("", lang_m)]) in
                self#register prepend;
                if not prepend#is_ready then (
                  self#log#important "Candidate to prepending not ready. Abort!";
                  state <- `Buffer peek;
                  self#unregister prepend )
                else state <- `Prepend (prepend, peek);
                self#get_frame buf ) )
        | `Buffer peek ->
            let p = AFrame.position buf in
            let pcm = AFrame.content buf p in
            let peek_pcm = AFrame.content peek p in
            let peekpos = AFrame.size () - 1 in
            for i = 0 to Array.length pcm - 1 do
              pcm.(i).{p} <- peek_pcm.(i).{peekpos}
            done;
            begin
              match AFrame.get_metadata peek peekpos with
              | Some m -> AFrame.set_metadata buf p m
              | None -> ()
            end;
            state <- `Replay;
            AFrame.add_break buf (p + 1);
            self#get_frame buf;
            AFrame.set_breaks buf
              (List.filter (( <> ) (p + 1)) (AFrame.breaks buf))
        | `Replay ->
            source#get buf;
            if AFrame.is_partial buf then state <- `Idle
        | `Prepend (prepend, peek) ->
            prepend#get buf;
            if AFrame.is_partial buf then (
              self#unregister prepend;
              state <- `Buffer peek;
              if merge then (
                let pos = AFrame.position buf in
                self#get_frame buf;
                AFrame.set_breaks buf
                  (Utils.remove_one (( = ) pos) (AFrame.breaks buf)) ) )

    method stype = source#stype

    method is_ready =
      match state with
        | `Idle | `Replay | `Buffer _ -> source#is_ready
        | `Prepend (s, _) -> s#is_ready || source#is_ready

    method remaining =
      match state with
        | `Idle | `Replay | `Buffer _ -> source#remaining
        | `Prepend (s, _) ->
            let ( + ) a b = if a < 0 || b < 0 then -1 else a + b in
            if merge then s#remaining + source#remaining else s#remaining

    method self_sync =
      match state with `Prepend (s, _) -> s#self_sync | _ -> source#self_sync

    (* Other behaviours could be wanted, but for now #abort_track won't abort
     * the prepended track. *)
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
        | `Prepend (p, _) -> self#unregister p
        | _ -> ()
      end;
      state <- `Idle

    method private register a = a#get_ready activation

    method private unregister a = a#leave (self :> source)
  end

let register =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "prepend"
    [
      ( "merge",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Merge the track with its appended track." );
      ("", Lang.source_t k, None, None);
      ( "",
        Lang.fun_t [(false, "", Lang.metadata_t)] (Lang.source_t k),
        None,
        Some
          "Given the metadata, build the source producing the track to \
           prepend. This source is allowed to fail (produce nothing) if no \
           relevant track is to be appended. However, success must be \
           immediate or it will not be taken into account." );
    ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.TrackProcessing
    ~descr:
      ( "Prepend an extra track before every track. "
      ^ "Set the metadata 'liq_prepend' to 'false' to "
      ^ "inhibit prepending on one track." )
    (fun p kind ->
      let merge = Lang.to_bool (Lang.assoc "merge" 1 p) in
      let source = Lang.to_source (Lang.assoc "" 1 p) in
      let f = Lang.assoc "" 2 p in
      new prepend ~kind ~merge source f)
