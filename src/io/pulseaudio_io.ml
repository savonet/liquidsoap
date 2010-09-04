(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

open Pulseaudio

(** Dedicated clock. *)
let get_clock = Tutils.lazy_cell (fun () -> new Clock.self_sync "pulse")

class virtual base p =
  let client = Lang.to_string (List.assoc "client" p) in
  let device = Lang.to_string (List.assoc "device" p) in
  let device = 
    if device = "" then None else Some device 
  in
object (self)
  val client_name = client
  val dev = device

  method virtual log : Dtools.Log.t
end

class output ~kind p =
  let source_val = List.assoc "" p in
  let source = Lang.to_source source_val in
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
object (self)

  initializer
    (* We need the source to be infallible. *)
    if source#stype <> Source.Infallible then
      raise (Lang.Invalid_value (source_val, "That source is fallible"))

  inherit Source.active_operator kind [source] as super
  inherit base p

  method set_clock =
    super#set_clock ;
    if Lang.to_bool (List.assoc "clock_safe" p) then
      let clock = get_clock () in
        Clock.unify self#clock (Clock.create_known (clock:>Clock.clock)) ;
        (* TODO in the future we should use the Output class to have
         * a start/stop behavior; until then we register once for all,
         * which is a quick but dirty solution. *)
        clock#register_blocking_source

  val mutable stream = None

  method stype = Source.Infallible
  method is_ready = true
  method remaining = source#remaining
  method get_frame buf = source#get buf
  method abort_track = source#abort_track

  method output_get_ready =
    let ss =
      {
        sample_format = Sample_format_float32le;
        sample_rate = samples_per_second;
        sample_chans = channels;
      }
    in
      stream <-
        Some (Pulseaudio.Simple.create
                ~client_name
                ~stream_name:self#id
                ?dev
                ~dir:Dir_playback
                ~sample:ss ());

  method output_reset = ()
  method is_active = true
 
  method output =
    while Frame.is_partial memo do
      source#get memo
    done;
    let stream = Utils.get_some stream in
    let buf = AFrame.content memo 0 in
      Simple.write stream buf 0 (Array.length buf.(0))
end

class input ~kind p =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
object (self)

  inherit Source.active_source kind as super
  inherit base p

  method set_clock =
    super#set_clock ;
    if Lang.to_bool (List.assoc "clock_safe" p) then
      let clock = get_clock () in
        Clock.unify self#clock (Clock.create_known (clock:>Clock.clock)) ;
        (* TODO in the future we should use the Output class to have
         * a start/stop behavior; until then we register once for all,
         * which is a quick but dirty solution. *)
        clock#register_blocking_source

  val mutable stream = None

  method stype = Source.Infallible
  method is_ready = true
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_get_ready =
    let ss =
      {
        sample_format = Sample_format_float32le;
        sample_rate = samples_per_second;
        sample_chans = channels;
      }
    in
      stream <- 
          Some (Pulseaudio.Simple.create ~client_name
                                         ~stream_name:self#id 
                                         ~dir:Dir_record 
                                         ?dev
                                         ~sample:ss ());

  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let stream = Utils.get_some stream in
    let buf = AFrame.content_of_type ~channels frame 0 in
      Simple.read stream buf 0 (Array.length buf.(0));
      AFrame.add_break frame (AFrame.size ())
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~audio:1 ()) in
  let proto =
    [ "client", Lang.string_t, 
        Some (Lang.string "liquidsoap"), None ;
      "device", Lang.string_t,
        Some (Lang.string ""), 
        Some "Device to use. Uses default if set to \"\"." ;
      "clock_safe", Lang.bool_t,
        Some (Lang.bool true),
        Some "Force the use of the dedicated Pulseaudio clock." ]
  in
  Lang.add_operator "output.pulseaudio"
    (proto @ ["", Lang.source_t k, None, None])
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Output
    ~descr:"Output the source's stream to a portaudio output device."
    (fun p kind ->
         ((new output ~kind p):>Source.source)) ;
  Lang.add_operator "input.pulseaudio"
    proto
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~descr:"Stream from a portaudio input device."
    (fun p kind -> ((new input ~kind p):>Source.source))
