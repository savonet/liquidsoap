(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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

class rms ~kind duration source =
  let channels = (Frame.type_of_kind kind).Frame.audio in
object (self)
  inherit operator kind [source] ~name:"rms" as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  (** Sum of squares. *)
  val sq = Array.create channels 0.
  (** Duration of the sum of squares in samples. *)
  val mutable sq_dur = 0
  (** Last computed rms. *)
  val mutable rms = Array.create channels 0.

  val m = Mutex.create ()

  method rms = Tutils.mutexify m (fun () -> rms) ()

  method private get_frame buf =
    let offset = AFrame.position buf in
    source#get buf;
    let duration = duration () in
    if duration > 0. then
      let duration = Frame.audio_of_seconds duration in
      let position = AFrame.position buf in
      let buf = AFrame.content buf offset in
      for i = offset to position - 1 do
        for c = 0 to channels - 1 do
          let x = buf.(c).(i) in
          sq.(c) <- sq.(c) +. x *. x
        done;
        sq_dur <- sq_dur + 1;
        if sq_dur >= duration then
          let dur = float sq_dur in
          let rms' = Array.init channels
            (fun i ->
              let r = sqrt (sq.(i) /. dur) in
              sq.(i) <- 0.;
              r)
          in
          sq_dur <- 0;
          Tutils.mutexify m (fun () -> rms <- rms') ()
      done
end

let declare suffix format fun_ret_t f_rms =
  let k = Lang.kind_type_of_kind_format ~fresh:3 format in
  let return_t =
    Lang.product_t
      (Lang.fun_t [] fun_ret_t)
      (Lang.source_t k)
  in
  Lang.add_builtin ("rms"^suffix)
    ~category:(Lang.string_of_category Lang.Visualization)
    ~descr:"Get current audio RMS volume of the source. \
            Returns a pair @(f,s)@ where s is a new source and \
            @f@ is a function of type @() -> float@ and \
            returns the current RMS of the source."
    [
      "id", Lang.string_t,Some (Lang.string ""), Some "Force the value of the source ID.";
      "duration", Lang.float_getter_t 2, Some (Lang.float 0.5), Some "Duration of the RMS window (in seconds). A value <= 0, means that RMS computation should not be performed.";
      "", Lang.source_t k, None, None
    ]
    return_t
    (fun p t ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let id = Lang.to_string (f "id") in
      let duration = Lang.to_float_getter (f "duration") in
      let (_,t) = Lang.of_product_t t in
      let kind = Lang.frame_kind_of_kind_type (Lang.of_source_t t) in
      let s = new rms ~kind duration src in
      if id <> "" then s#set_id id;
      let f = Lang.val_fun [] ~ret_t:fun_ret_t (fun p t -> f_rms s#rms) in
      Lang.product f (Lang.source (s :> Source.source)))

let () =
  let f rms =
    let r = Array.fold_left (+.) 0. rms in
    let r = r /. float (Array.length rms) in
    Lang.float r
  in
  let f_stereo rms =
    Lang.product (Lang.float rms.(0)) (Lang.float rms.(1))
  in
  declare "" Lang.any_fixed Lang.float_t f;
  declare ".stereo" (Lang.any_fixed_with ~audio:2 ()) (Lang.product_t Lang.float_t Lang.float_t) f_stereo
