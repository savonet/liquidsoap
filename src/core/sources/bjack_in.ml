(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Mm

let log = Log.make ["input"; "jack"]

module SyncSource = Clock.MkSyncSource (struct
  type t = unit

  let to_string _ = "jack"
end)

let sync_source = SyncSource.make ()

class jack_in ~self_sync ~fallible ~autostart ~server =
  let samples_per_frame = AFrame.size () in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let bytes_per_sample = 2 in

  object (self)
    inherit
      Start_stop.active_source ~name:"input.jack" ~fallible ~autostart () as active_source

    method seek_source = (self :> Source.source)
    method private can_generate_frame = active_source#started
    method abort_track = ()
    method remaining = -1
    val mutable sample_freq = samples_per_second
    val mutable device = None

    method self_sync =
      if self_sync then
        (`Dynamic, if device <> None then Some sync_source else None)
      else (`Static, None)

    method stop =
      match device with
        | Some d ->
            Bjack.close d;
            device <- None
        | None -> ()

    initializer self#on_sleep (fun () -> self#stop)
    method start = ignore self#get_device

    method private get_device =
      match device with
        | None ->
            let server_name = match server with "" -> None | s -> Some s in
            let dev =
              try
                Bjack.open_t ~rate:samples_per_second
                  ~bits_per_sample:(bytes_per_sample * 8)
                  ~input_channels:self#audio_channels ~output_channels:0
                  ~flags:[] ?server_name
                  ~ringbuffer_size:(samples_per_frame * bytes_per_sample)
                  ~client_name:self#id ()
              with Bjack.Open ->
                failwith "Could not open JACK device: is the server running?"
            in
            Bjack.set_all_volume dev 100;
            device <- Some dev;
            dev
        | Some d -> d

    val cache = Strings.Mutable.empty ()

    method private read_data blen =
      let dev = self#get_device in
      while Strings.Mutable.length cache < blen do
        Strings.Mutable.add cache (Bjack.read dev blen)
      done

    method private generate_frame =
      let length = Lazy.force Frame.size in
      let alen = Frame.audio_of_main length in
      let blen = Audio.S16LE.size self#audio_channels alen in
      self#read_data blen;
      let pcm = Strings.Mutable.(to_string (sub cache 0 blen)) in
      Strings.Mutable.drop cache blen;
      let frame = Frame.create ~length self#content_type in
      let buf = Content.Audio.get_data (Frame.get frame Frame.Fields.audio) in
      Audio.S16LE.to_audio pcm 0 buf 0 alen;
      Frame.set_data frame Frame.Fields.audio Content.Audio.lift_data buf

    method! reset = ()
  end

let _ =
  let return_t =
    Lang.frame_t Lang.unit_t
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.input "jack"
    (Start_stop.active_source_proto ~fallible_opt:(`Yep false)
    @ [
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Mark the source as being synchronized by the jack server." );
        ( "server",
          Lang.string_t,
          Some (Lang.string ""),
          Some "Jack server to connect to." );
      ])
    ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"source")
    ~return_t ~category:`Input ~descr:"Get stream from jack."
    (fun p ->
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let fallible = Lang.to_bool (List.assoc "fallible" p) in
      let autostart = Lang.to_bool (List.assoc "start" p) in
      let server = Lang.to_string (List.assoc "server" p) in
      (new jack_in ~self_sync ~server ~fallible ~autostart
        :> Start_stop.active_source))
