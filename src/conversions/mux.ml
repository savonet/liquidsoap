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

(** Muxing takes a master and an auxiliary source.
  * The auxiliary source streams only one kind of content,
  * the master has no channel of that kind, anything for the others.
  *
  * There are several possible modes for muxing:
  *  - Master: the auxiliary source should be infallible, and has to
  *    fill in exactly the data zone that the master produces.
  *    Track information for the aux source is lost.
  *    In the future we might want to require exclusivity on the auxiliary
  *    source because this mode requires doing tricks to it, and sharing
  *    would have funny effects.
  *  - Auxiliary: same with exchanged roles.
  *  - None: The sources have a symmetric role, we loose all track
  *    information, filling as much as possible. If one of the sources
  *    is not ready anymore, extra data from the other is dropped.
  *  - Both: We get tracks from both sources.
  *)
type mode = Merge | Master | Auxiliary | Both

let mode_of_string = function
  | "none" -> Merge
  | "master" -> Master
  | "auxiliary" -> Auxiliary
  | "both" -> Both
  | s -> failwith ("Unknown track mode: " ^ s)

let get_mode p = mode_of_string (Lang.to_string (List.assoc "mode" p))

let mode_help =
  "Track mode, specifying where the tracks are taken from \
   (none|master|auxiliary|both). Modes other than `\"none\"` might induce a \
   slight desynchronization between sources."

class mux ~kind ~mode ~master ~master_layer ~aux ~aux_layer mux_content =
  let dest_type = Frame.type_of_kind kind in
  object (self)
    inherit Source.operator ~name:"mux" kind [master; aux]

    method self_sync = master#self_sync || aux#self_sync

    method stype =
      if master#stype = Source.Infallible && aux#stype = Source.Infallible then
        Source.Infallible
      else Source.Fallible

    method is_ready = master#is_ready && aux#is_ready

    method abort_track =
      master#abort_track;
      aux#abort_track

    method remaining =
      let master = master#remaining in
      let aux = aux#remaining in
      if master = -1 && aux = -1 then -1 else min master aux

    method private get_frame frame =
      let pos = Frame.position frame in
      (* Immediately get the final destination frame. It will be used to obtain
       data from aux and master sources directly at the right place, so that
       muxing only consists in putting channels together, without the need for
       blitting. It remains possible that one of our sources ends up writing in
       another content layer than the expected one. So we call blit just in
       case; it doesn't cost anything when the array are already identical. *)
      let dest = Frame.content_of_type frame pos dest_type in
      let get until adapt_layer s =
        let breaks = Frame.breaks frame in
        let inicon = adapt_layer dest in
        let inicon =
          Frame.content_of_type ~force:inicon frame pos
            (Frame.type_of_content inicon)
        in
        let once = ref true in
        let more () =
          match until with
            | `Once ->
                let b = !once in
                once := false;
                b
            | `Full -> true
            | `Pos p -> Frame.position frame < p
        in
        while s#is_ready && Frame.is_partial frame && more () do
          s#get frame
        done;
        let _, c = Frame.content frame pos in
        let end_pos = Frame.position frame in
        if inicon != c then
          self#log#debug "Copy-avoiding optimization isn't working!";
        Frame.set_breaks frame breaks;
        (c, end_pos)
      in
      (* Hiding contents to avoid the following. If the master layer is present in
       the frame, it may be used by one of the sources. For example, master is
       stereo, we call the aux source with a mono layer (half of the stereo one)
       but that source is a mean so its sub-source will write on the underlying
       stereo layer (before computing the mean in the mono layer) overwriting
       the data written before by the master source.  For similar reasons we
       hide what the master has written before the aux source gets the frame. *)
      let restore = Frame.hide_contents frame in
      let new_content, end_pos =
        match mode with
          | Merge ->
              (* We get as much info as possible from one source, save the content,
            repeat the operation for the other, then merge the contents. If one
            source produces more than the other, extra data is dropped. This
            could be avoided if we could get sample by sample, or know in
            advance how many samples each source can produce. *)
              let master, end_master = get `Full master_layer master in
              let (_ : unit -> unit) = Frame.hide_contents frame in
              let aux, end_aux = get `Full aux_layer aux in
              let end_pos = min end_master end_aux in
              let new_content = mux_content master aux in
              (new_content, end_pos)
          | Both ->
              let master, end_master = get `Once master_layer master in
              let (_ : unit -> unit) = Frame.hide_contents frame in
              let aux, end_aux = get `Once aux_layer aux in
              let end_pos = min end_master end_aux in
              let new_content = mux_content master aux in
              (new_content, end_pos)
          | Master ->
              (* The aux tries to produce as much as the master. Drop if too much. *)
              let master, end_master = get `Once master_layer master in
              let (_ : unit -> unit) = Frame.hide_contents frame in
              let aux, end_aux = get (`Pos end_master) aux_layer aux in
              let end_pos = min end_master end_aux in
              let new_content = mux_content master aux in
              (new_content, end_pos)
          | Auxiliary ->
              (* The opposite *)
              let aux, end_aux = get `Once aux_layer aux in
              let (_ : unit -> unit) = Frame.hide_contents frame in
              let master, end_master = get (`Pos end_aux) master_layer master in
              let end_pos = min end_master end_aux in
              let new_content = mux_content master aux in
              (new_content, end_pos)
      in
      restore ();
      Frame.blit_content new_content pos dest pos (end_pos - pos);
      Frame.add_break frame end_pos
  end

let () =
  let out_t = Lang.kind_type_of_kind_format Lang.any_fixed in
  let { Frame.audio; video; midi } = Lang.of_frame_kind_t out_t in
  let master_t = Lang.frame_kind_t ~audio ~video:Lang.zero_t ~midi in
  let aux_t = Lang.frame_kind_t ~audio:Lang.zero_t ~video ~midi:Lang.zero_t in
  Lang.add_operator "mux_video" ~category:Lang.Conversions
    ~descr:"Add video channnels to a stream." ~kind:(Lang.Unconstrained out_t)
    [
      ("mode", Lang.string_t, Some (Lang.string "master"), Some mode_help);
      ("video", Lang.source_t aux_t, None, None);
      ("", Lang.source_t master_t, None, None);
    ]
    (fun p kind ->
      let master = Lang.to_source (List.assoc "" p) in
      let master_layer c = { c with Frame.video = [||] } in
      let aux = Lang.to_source (List.assoc "video" p) in
      let aux_layer c = { c with Frame.audio = [||]; midi = [||] } in
      let mux_content master aux =
        { master with Frame.video = aux.Frame.video }
      in
      let mode = get_mode p in
      new mux ~kind ~mode ~master ~aux ~master_layer ~aux_layer mux_content)

let () =
  let out_t = Lang.kind_type_of_kind_format Lang.any_fixed in
  let { Frame.audio; video; midi } = Lang.of_frame_kind_t out_t in
  let master_t = Lang.frame_kind_t ~audio:Lang.zero_t ~video ~midi in
  let aux_t = Lang.frame_kind_t ~audio ~video:Lang.zero_t ~midi:Lang.zero_t in
  Lang.add_operator "mux_audio" ~category:Lang.Conversions
    ~descr:"Mux an audio stream into an audio-free stream."
    ~kind:(Lang.Unconstrained out_t)
    [
      ("mode", Lang.string_t, Some (Lang.string "master"), Some mode_help);
      ("audio", Lang.source_t aux_t, None, None);
      ("", Lang.source_t master_t, None, None);
    ]
    (fun p kind ->
      let master = Lang.to_source (List.assoc "" p) in
      let master_layer c = { c with Frame.audio = [||] } in
      let aux = Lang.to_source (List.assoc "audio" p) in
      let aux_layer c = { c with Frame.video = [||]; midi = [||] } in
      let mux_content master aux =
        { master with Frame.audio = aux.Frame.audio }
      in
      let mode = get_mode p in
      new mux ~kind ~mode ~master ~aux ~master_layer ~aux_layer mux_content)

let add_audio_mux label n =
  let master_t = Lang.kind_type_of_kind_format Lang.any_fixed in
  let aux_t =
    Lang.frame_kind_t ~audio:(Lang.type_of_int n) ~video:Lang.zero_t
      ~midi:Lang.zero_t
  in
  let out_t =
    let { Frame.audio; video; midi } = Lang.of_frame_kind_t master_t in
    Lang.frame_kind_t ~audio:(Lang.add_t n audio) ~video ~midi
  in
  Lang.add_operator ("mux_" ^ label) ~category:Lang.Conversions
    ~descr:("Mux a " ^ label ^ " audio stream into another stream.")
    ~kind:(Lang.Unconstrained out_t)
    [
      ("mode", Lang.string_t, Some (Lang.string "master"), Some mode_help);
      (label, Lang.source_t aux_t, None, None);
      ("", Lang.source_t master_t, None, None);
    ]
    (fun p kind ->
      let master = Lang.to_source (List.assoc "" p) in
      let aux = Lang.to_source (List.assoc label p) in
      let master_layer c =
        {
          c with
          Frame.audio =
            Array.sub c.Frame.audio n (Array.length c.Frame.audio - n);
        }
      in
      let aux_layer c =
        { Frame.audio = Array.sub c.Frame.audio 0 n; video = [||]; midi = [||] }
      in
      let mux_content master aux =
        let audio =
          Array.init
            (n + Array.length master.Frame.audio)
            (fun i ->
              if i < n then aux.Frame.audio.(i) else master.Frame.audio.(i - n))
        in
        { master with Frame.audio }
      in
      let mode = get_mode p in
      new mux ~kind ~mode ~master ~aux ~master_layer ~aux_layer mux_content)

let () =
  add_audio_mux "mono" 1;
  add_audio_mux "stereo" 2
