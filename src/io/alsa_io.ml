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

open Alsa

exception Error of string

let handle lbl f x =
  try f x with
    | Unknown_error e ->
        failwith (Printf.sprintf "Error while setting %s: %s"
                    lbl (string_of_error e))
    | e ->
        failwith (Printf.sprintf "Error while setting %s: %s"
                    lbl (Printexc.to_string e))

class virtual base ~kind dev mode =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let samples_per_frame = AFrame.size () in
object (self)
  initializer
    (Dtools.Conf.as_bool (Configure.conf#path ["root";"sync"]))#set false

  method virtual log : Dtools.Log.t

  val mutable alsa_rate = -1

  val mutable pcm = None

  val mutable write =
    (fun pcm buf ofs len -> Pcm.writen_float pcm buf ofs len)

  val mutable read =
    (fun pcm buf ofs len -> Pcm.readn_float pcm buf ofs len)

  method output_get_ready =
    self#log#f 3 "Using ALSA %s." (Alsa.get_version ()) ;
    try
      let dev =
        match pcm with
          | None -> handle "open_pcm" (Pcm.open_pcm dev mode) []
          | Some d -> d
      in
      let params = Pcm.get_params dev in

        (
          try
            handle "access"
              (Pcm.set_access dev params) Pcm.Access_rw_noninterleaved;
            handle "format"
              (Pcm.set_format dev params) Pcm.Format_float
          with
            | _ ->
                (* If we can't get floats we fallback on interleaved s16le *)
                self#log#f 2 "Falling back on interleaved S16LE";
                handle "format" (Pcm.set_format dev params) Pcm.Format_s16_le;
                (
                  try
                    Pcm.set_access dev params Pcm.Access_rw_interleaved;
                    write <-
                    (fun pcm buf ofs len ->
                       let sbuf = String.create (2 * len * Array.length buf) in
                       let _ =  Float_pcm.to_s16le buf ofs len sbuf 0 in
                         Pcm.writei pcm sbuf 0 len
                    );
                    read <-
                    (fun pcm buf ofs len ->
                       let sbuf = String.create (2 * 2 * len) in
                       let r = Pcm.readi pcm sbuf 0 len in
                         Float_pcm.from_s16le buf ofs sbuf 0 r;
                         r
                    )
                  with
                    | Alsa.Invalid_argument ->
                        self#log#f 2 "Falling back on non-interleaved S16LE";
                        handle "access"
                          (Pcm.set_access dev params)
                          Pcm.Access_rw_noninterleaved;
                        write <-
                        (fun pcm buf ofs len ->
                           let sbuf =
                             Array.init
                               channels
                               (fun _ -> String.create (2 * len))
                           in
                           let _ =  Float_pcm.to_s16le_ni buf ofs len sbuf 0 in
                             Pcm.writen pcm sbuf 0 len
                        );
                        read <-
                        (fun pcm buf ofs len ->
                           let sbuf =
                             Array.init
                               channels
                               (fun _ -> String.create (2 * len))
                           in
                           let r = Pcm.readn pcm sbuf 0 len in
                             Float_pcm.from_s16le_ni buf ofs sbuf 0 r;
                             r
                        )
                );
        );
        handle "channels"
          (Pcm.set_channels dev params) channels ;
        handle "periods"
          (Pcm.set_periods dev params Alsa_settings.periods#get) Dir_eq ;
        let rate =
          handle "rate" (Pcm.set_rate_near dev params samples_per_second) Dir_eq
        in
        let bufsize =
          handle "buffer size"
            (Pcm.set_buffer_size_near dev params) samples_per_frame
        in
          alsa_rate <- rate;
          if rate <> samples_per_second then
            self#log#f 3
              "Could not set sample rate to 'frequency' (%d Hz), got %d."
              samples_per_second rate ;
          if bufsize <> samples_per_frame then
            self#log#f 3
              "Could not set buffer size to 'frame.size' (%d samples), got %d."
              samples_per_frame bufsize ;

          (
          try
            Pcm.set_params dev params
          with
            | Alsa.Invalid_argument as e ->
                self#log#f 1
                  "Setting alsa parameters failed (invalid argument)!";
                raise e
          );
          handle "non-blocking" (Pcm.set_nonblock dev) false ;
          pcm <- Some dev
    with
      | Unknown_error n -> raise (Error (string_of_error n))

  method output_reset = ()
  method is_active = true
end

class output ~kind dev val_source =
  let source = Lang.to_source val_source in
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_second = Lazy.force Frame.audio_rate in
object (self)
  inherit Source.active_operator kind [source]
  inherit base ~kind dev [Pcm.Playback]

  initializer
    self#set_id (Printf.sprintf "alsa_out(%s)" dev) ;
    (* We need the source to be infallible. *)
    if source#stype <> Source.Infallible then
      raise (Lang.Invalid_value (val_source, "That source is fallible"))

  val samplerate_converter = Audio_converter.Samplerate.create channels

  method stype = Source.Infallible
  method is_ready = true
  method remaining = source#remaining
  method get_frame buf = source#get buf
  method abort_track = source#abort_track

  method output =
    while Frame.is_partial memo do
      source#get memo
    done ;
    let pcm = Utils.get_some pcm in
    let buf = AFrame.content memo 0 in
    let buf =
      if alsa_rate = samples_per_second then
        buf
      else
        Audio_converter.Samplerate.resample
          samplerate_converter
          (float alsa_rate /. float samples_per_second)
          buf 0 (Array.length buf)
    in
      try
        let r = ref 0 in
          while !r < Array.length buf.(0) do
            if !r <> 0 then
              self#log#f 4
                "Partial write (%d instead of %d)! \
                 Selecting another buffer size or device can help."
                !r (Array.length buf.(0));
            r := !r + (write pcm buf !r (Array.length buf.(0) - !r))
          done
      with
        | Buffer_xrun ->
            self#log#f 2
              "Underrun! You may minimize them by increasing the buffer size." ;
            Pcm.prepare pcm ; Pcm.reset pcm ;
            self#output
        | Unknown_error e -> raise (Error (string_of_error e))

end

class input ~kind dev =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_frame = AFrame.size () in
object (self)
  inherit Source.active_source kind
  inherit base ~kind dev [Pcm.Capture]

  method stype = Source.Infallible
  method is_ready = true
  method remaining = -1
  method abort_track = ()
  method output = if AFrame.is_partial memo then self#get_frame memo

  (* TODO: convert samplerate *)
  method get_frame frame =
    assert (0 = AFrame.position frame) ;
    let pcm = Utils.get_some pcm in
    let buf = AFrame.content_of_type ~channels frame 0 in
      try
        let r = ref 0 in
          while !r < samples_per_frame do
            if !r <> 0 then
              self#log#f 4
                   "Partial read (%d instead of %d)! \
                    Selecting another buffer size or device can help."
                !r (Array.length buf.(0));
            r := !r + (read pcm buf !r (samples_per_frame - !r))
          done;
          AFrame.add_break frame (AFrame.size ())
      with
        | Buffer_xrun ->
            self#log#f 2
              "Overrun! You may minimize them by increasing the buffer size." ;
            Pcm.prepare pcm ; Pcm.reset pcm ;
            self#get_frame frame
        | Unknown_error e -> raise (Error (string_of_error e))

end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~audio:1 ()) in
  Lang.add_operator "output.alsa"
    [
      "bufferize", Lang.bool_t, Some (Lang.bool true), Some "Bufferize output";

      "device", Lang.string_t, Some (Lang.string "default"),
      Some "Alsa device to use";

      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Output
    ~descr:"Output the source's stream to an ALSA output device."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let bufferize = e Lang.to_bool "bufferize" in
       let device = e Lang.to_string "device" in
       let source = List.assoc "" p in
         if bufferize then
           (* TODO: add a parameter for autostart? *)
           ((new Alsa_out.output ~kind device true source):>Source.source)
         else
           ((new output ~kind device source):>Source.source)
    ) ;
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.audio_any in
  Lang.add_operator "input.alsa"
    [
      "bufferize", Lang.bool_t, Some (Lang.bool true), Some "Bufferize input.";

      "device", Lang.string_t, Some (Lang.string "default"),
      Some "Alsa device to use."
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Input
    ~descr:"Stream from an ALSA input device."
    (fun p kind ->
       let e f v = f (List.assoc v p) in
       let bufferize = e Lang.to_bool "bufferize" in
       let device = e Lang.to_string  "device" in
         if bufferize then
           ((new Alsa_in.mic ~kind device):>Source.source)
         else
           ((new input ~kind device):>Source.source)
    ) ;
