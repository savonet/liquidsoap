(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

class virtual base =
object (self)
  initializer
    (* We are using blocking functions to read/write. *)
    (Dtools.Conf.as_bool (Configure.conf#path ["root";"sync"]))#set false

  method virtual log : Dtools.Log.t
end

class output val_source =
  let source = Lang.to_source val_source in
  let channels = Fmt.channels () in
  let samples_per_second = Fmt.samples_per_second () in
object (self)
  inherit Source.active_operator source
  inherit base

  initializer
    (* We need the source to be infallible. *)
    if source#stype <> Source.Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  val mutable stream = None

  method stype = Source.Infallible
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
      stream <- Some (Pulseaudio.Simple.create ~client_name:"liquidsoap" ~stream_name:"liq stream" ~dir:Dir_playback ~sample:ss ());

  method output_reset = ()

  method output =
    while Frame.is_partial memo do
      source#get memo
    done;
    let stream = Utils.get_some stream in
    let buf = AFrame.get_float_pcm memo in
      Simple.write stream buf 0 (Array.length buf.(0))
end

class input =
  let channels = Fmt.channels () in
  let samples_per_second = Fmt.samples_per_second () in
object (self)
  inherit Source.active_source
  inherit base

  val mutable stream = None

  method stype = Source.Infallible
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
      stream <- Some (Pulseaudio.Simple.create ~client_name:"liquidsoap" ~stream_name:"liq stream" ~dir:Dir_record ~sample:ss ());

  method output_reset = ()

  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let stream = Utils.get_some stream in
    let buf = AFrame.get_float_pcm frame in
      Simple.read stream buf 0 (Array.length buf.(0));
      AFrame.add_break frame (AFrame.size frame)
end

let () =
  Lang.add_operator "output.pulseaudio"
    [
      "", Lang.source_t, None, None
    ]
    ~category:Lang.Output
    ~descr:"Output the source's stream to a portaudio output device."
    (fun p ->
       (* let e f v = f (List.assoc v p) in *)
       let source = List.assoc "" p in
         ((new output source):>Source.source)
    );
  Lang.add_operator "input.pulseaudio"
    [
    ]
    ~category:Lang.Input
    ~descr:"Stream from a portaudio input device."
    (fun p ->
       (* let e f v = f (List.assoc v p) in *)
       ((new input):>Source.source)
    );
