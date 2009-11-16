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

external set_format : Unix.file_descr -> int -> int = "caml_oss_dsp_setfmt"

external set_channels : Unix.file_descr -> int -> int = "caml_oss_dsp_channels"

external set_rate : Unix.file_descr -> int -> int = "caml_oss_dsp_speed"

class output ~kind dev val_source =
  let source = Lang.to_source val_source in
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
object (self)
  inherit Source.active_operator kind source

  initializer
    (* We need the source to be infallible. *)
    if source#stype <> Source.Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  val mutable fd = None

  method stype = Source.Infallible
  method is_ready = true
  method remaining = source#remaining
  method get_frame buf = source#get buf
  method abort_track = source#abort_track

  method output_get_ready =
    fd <- Some (Unix.openfile dev [Unix.O_WRONLY] 0o200);
    let fd = Utils.get_some fd in
      assert (set_format fd 16 = 16);
      assert (set_channels fd channels = channels);
      assert (set_rate fd samples_per_second = samples_per_second)

  method output_reset = ()
  method is_active = true

  method output =
    while Frame.is_partial memo do
      source#get memo
    done;
    let fd = Utils.get_some fd in
    let buf = AFrame.content memo 0 in
    let s = String.create (2 * (Array.length buf) * (Array.length buf.(0))) in
    let r = Float_pcm.to_s16le buf 0 (Array.length buf.(0)) s 0 in
      assert (Unix.write fd s 0 r = r)
end

class input ~kind dev =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
object (self)
  inherit Source.active_source kind

  val mutable fd = None

  method stype = Source.Infallible
  method is_ready = true
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  method output_get_ready =
    fd <- Some (Unix.openfile dev [Unix.O_RDONLY] 0o400);
    let fd = Utils.get_some fd in
      assert (set_format fd 16 = 16);
      assert (set_channels fd channels = channels);
      assert (set_rate fd samples_per_second = samples_per_second)

  method output_reset = ()
  method is_active = true

  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let fd = Utils.get_some fd in
    let buf = AFrame.content_of_type ~channels frame 0 in
    let len = 2 * (Array.length buf) * (Array.length buf.(0)) in
    let s = String.create len in
    let r = Unix.read fd s 0 len in
      (* TODO: recursive read ? *)
      assert (len = r) ;
      Float_pcm.from_s16le buf 0 s 0 (Array.length buf.(0));
      AFrame.add_break frame (AFrame.size ())
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~audio:1 ()) in
  Lang.add_operator "output.oss"
    [
      "device", Lang.string_t, Some (Lang.string "/dev/dsp"),
      Some "OSS device to use.";
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Output
    ~descr:"Output the source's stream to an OSS output device."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let device = e Lang.to_string "device" in
       let source = List.assoc "" p in
         ((new output ~kind device source):>Source.source)
    );
  Lang.add_operator "input.oss"
    [
      "device", Lang.string_t, Some (Lang.string "/dev/dsp"),
      Some "OSS device to use.";
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~descr:"Stream from an OSS input device."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let device = e Lang.to_string "device" in
         ((new input ~kind device):>Source.source)
    )
