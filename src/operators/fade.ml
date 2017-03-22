(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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

let get_fader name =
  let wrap f =
    Some (fun l ->
      let l = float l in
       fun i ->
         let i = float i /. l in
          f (max 0. (min 1. i)))
  in
  (* A few typical shapes..
   * In theory, any mapping from [0:1] to [0:1] is OK,
   * preferably monotonic and one-to-one. *)
  match name with
    | "lin" -> wrap (fun x -> x)
    | "log" ->
        let curve = 10. in
        let m = log (1.+.curve) in
        wrap (fun x -> log (1. +. x*.10.) /. m)
    | "exp" ->
        let curve = 2. in
        let m = exp curve -. 1. in
        wrap (fun x -> (exp (curve *. x) -. 1.) /. m)
    | "sin" ->
        let pi = acos (-1.) in
        wrap (fun x -> (1. +. sin ((x-.0.5)*.pi))/.2.)
    | _ -> None

(** Fade-in at the beginning of every frame.
  * The [duration] is in seconds.
  * If the initial flag is set, only the first/current track is faded in. *)
class fade_in ~kind
  ~duration_meta ~type_meta ?(initial=false) duration fader source =
object(self)
  inherit operator ~name:"fade_in" kind [source]
  inherit Latest_metadata.source

  method stype = source#stype
  method is_ready = source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining

  val mutable state = `Idle

  val mutable fader = fader
  val mutable duration = duration

  method private on_new_metadata =
    begin
      match Utils.hashtbl_get latest_metadata type_meta with
        | Some n ->
            fader <- (try Utils.get_some (get_fader n) with _ -> fader)
        | None -> ()
    end;
    match Utils.hashtbl_get latest_metadata duration_meta with
      | Some d ->
          duration <- (try float_of_string d with _ -> duration)
      | None -> ()

  method private get_frame ab =
    let p1 = AFrame.position ab in
    let p2 = source#get ab ; AFrame.position ab in
    self#save_latest_metadata ab ;
    if Frame.is_partial ab then
      self#clear_latest_metadata ;
    (** In samples: [length] of the fade, [count] since beginning. *)
    let fade,length,count =
      match state with
        | `Idle ->
            let length = Frame.audio_of_seconds duration in
              fader length,
              length,
              0
        | `Play (fade,length,count) -> fade,length,count
    in
    let pcm = AFrame.content ab p1 in
      if count < length then
        for i=0 to min (p2-p1-1) (length-count-1) do
          let m = fade (count+i) in
            for c = 0 to Array.length pcm - 1 do
              pcm.(c).(p1+i) <- m *. pcm.(c).(p1+i)
            done
        done ;
      state <- (if not initial && AFrame.is_partial ab then
                  `Idle
                else
                  `Play (fade,length,count+p2-p1))

end

(** Fade-out after every frame.
  * If the final flag is set, the fade-out happens as of instantiation
  * and the source becomes unavailable once it's finished. *)
class fade_out ~kind
  ~duration_meta ~type_meta ?(final=false) duration fader source =
object
  inherit operator ~name:"fade_out" kind [source]
  inherit Latest_metadata.source as latest

  method stype = if final then Fallible else source#stype
  method abort_track = source#abort_track

  (* Fade-out length (in samples) for the current track.
   * The value is set at the beginning of every track,
   * depending on metadata. *)
  val mutable cur_length = None

  (* Remaining frames, used only in final mode, untouched otherwise. *)
  val mutable remaining = Frame.audio_of_seconds duration

  method remaining =
    if final then Frame.master_of_audio remaining else source#remaining

  method is_ready = (remaining > 0 || not final) && source#is_ready

  val mutable fader = fader
  val mutable duration = duration

  method private on_new_metadata =
    begin
      match Utils.hashtbl_get latest_metadata type_meta with
        | Some n ->
            fader <- (try Utils.get_some (get_fader n) with _ -> fader)
        | None -> ()
    end;
    match Utils.hashtbl_get latest_metadata duration_meta with
      | Some d ->
          duration <- (try float_of_string d with _ -> duration)
      | None -> ()

  method private get_frame ab =
    if final && remaining <= 0 then
      (* This happens in final mode at the end of the remaining time. *)
      Frame.add_break ab (Frame.position ab)
    else
      let n = Frame.audio_of_master source#remaining in
      let offset1 = AFrame.position ab in
      let offset2 = source#get ab ; AFrame.position ab in
      latest#save_latest_metadata ab ;
      (** In samples: [length] of the fade. *)
      let fade,length =
        match cur_length with
          | Some (f,l) -> f,l
          | None ->
              let l = Frame.audio_of_seconds duration in
              let f = fader l in
                cur_length <- Some (f,l) ;
                f,l
      in
        (* Process the buffer *)
        begin match
          if final then Some remaining else
            if n>=0 && n<length then Some n else None
        with
          | Some n ->
              let buffer = AFrame.content ab offset1 in
                for i=0 to offset2-offset1-1 do
                  let m = fade (n-i) in
                    for c=0 to Array.length buffer - 1 do
                      buffer.(c).(offset1+i) <- m *. buffer.(c).(offset1+i)
                    done
                done
          | None -> ()
        end ;
        if final then remaining <- remaining - offset2 + offset1 ;
        (* Reset the length at the end of a track *)
        if Frame.is_partial ab then cur_length <- None

end

(** Lang interface *)

let proto,kind =
  (* TODO check about sharing for the kind variables *)
  let kind = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  [ "duration", Lang.float_t, Some (Lang.float 3.),
     Some "Duration of the fading. \
           This value can be set on a per-file basis using the metadata field \
           passed as override." ;
    "type", Lang.string_t, Some (Lang.string "lin"),
    Some "Fader shape (lin|sin|log|exp): \
          linear, sinusoidal, logarithmic or exponential." ;
    "", Lang.source_t kind, None, None ],
  kind

let override_proto duration_name = proto @ [
  "override_duration", Lang.string_t, Some (Lang.string duration_name),
   Some "Metadata field which, if present and containing a float, \
         overrides the 'duration' parameter for current track.";
  "override_type", Lang.string_t, Some (Lang.string "liq_fade_type"),
   Some "Metadata field which, if present and correct, overrides the \
         'type' parameter for current track." ]


let extract p =
  Lang.to_float (List.assoc "duration" p),
  (let mode = List.assoc "type" p in
   (* A few typical shapes..
    * In theory, any mapping from [0:1] to [0:1] is OK,
    * preferably monotonic and one-to-one. *)
   match get_fader (Lang.to_string mode) with
     | Some f -> f
     | None ->
         let msg =
           "The 'type' parameter should be 'lin','sin','log' or 'exp'!"
         in
           raise (Lang.Invalid_value (mode,msg))),
  Lang.to_string (List.assoc "override_duration" p),
  Lang.to_string (List.assoc "override_type" p),
  Lang.to_source (List.assoc "" p)

let () =
  Lang.add_operator
    "fade.in" (override_proto "liq_fade_in")
    ~category:Lang.SoundProcessing
    ~descr:"Fade the beginning of tracks."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
       let d,f,duration_meta,type_meta,s = extract p in
       new fade_in ~kind ~duration_meta ~type_meta d f s) ;
  Lang.add_operator "fade.initial" (override_proto "liq_fade_initial")
    ~category:Lang.SoundProcessing
    ~descr:"Fade the beginning of a stream."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
       let d,f,duration_meta,type_meta,s = extract p in
       new fade_in ~kind ~initial:true ~duration_meta ~type_meta d f s) ;
  Lang.add_operator "fade.out" (override_proto "liq_fade_out")
    ~category:Lang.SoundProcessing
    ~descr:"Fade the end of tracks."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
       let d,f,duration_meta,type_meta,s = extract p in
       new fade_out ~kind ~duration_meta ~type_meta d f s) ;
  Lang.add_operator "fade.final" (override_proto "liq_fade_final")
    ~category:Lang.SoundProcessing
    ~descr:"Fade a stream to silence."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
       let d,f,duration_meta,type_meta,s = extract p in
       new fade_out ~kind ~duration_meta ~type_meta ~final:true d f s)
