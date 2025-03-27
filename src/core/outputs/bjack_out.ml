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

(** Output using ocaml-jack. *)

let bytes_per_sample = 2

class output ~self_sync ~infallible ~register_telnet ~on_stop ~on_start ~server
  source =
  let samples_per_frame = AFrame.size () in
  let seconds_per_frame = Frame.seconds_of_audio samples_per_frame in
  let samples_per_second = Lazy.force Frame.audio_rate in
  object (self)
    inherit
      Output.output
        ~infallible ~register_telnet ~on_stop ~on_start ~name:"output.jack"
          ~output_kind:"output.jack" source true

    val mutable device = None

    method self_sync =
      if self_sync then
        (`Dynamic, if device <> None then Some Bjack_in.sync_source else None)
      else (`Static, None)

    method get_device =
      match device with
        | None ->
            (* Wait for things to settle *)
            Thread.delay (5. *. seconds_per_frame);
            let server_name = match server with "" -> None | s -> Some s in
            let dev =
              try
                Bjack.open_t ~rate:samples_per_second
                  ~bits_per_sample:(bytes_per_sample * 8) ~input_channels:0
                  ~output_channels:self#audio_channels ~flags:[] ?server_name
                  ~ringbuffer_size:(samples_per_frame * bytes_per_sample)
                  ~client_name:self#id ()
              with Bjack.Open ->
                failwith "Could not open JACK device: is the server running?"
            in

            Bjack.set_all_volume dev 100;
            device <- Some dev;
            dev
        | Some d -> d

    method start = ignore self#get_device

    method stop =
      match device with
        | Some d ->
            Bjack.close d;
            device <- None
        | None -> ()

    method send_frame frame =
      let dev = self#get_device in
      let data = AFrame.s16le frame in
      let len = String.length data in
      let remaining = ref (len - Bjack.write dev data) in
      while !remaining > 0 do
        Thread.delay (seconds_per_frame /. 2.);
        let tmp = Str.string_after data (len - !remaining) in
        let written = Bjack.write dev tmp in
        remaining := !remaining - written
      done

    method! reset = ()
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.output "jack"
    (Output.proto
    @ [
        ( "self_sync",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Force the use of the dedicated bjack clock." );
        ( "server",
          Lang.string_t,
          Some (Lang.string ""),
          Some "Jack server to connect to." );
        ("", Lang.source_t frame_t, None, None);
      ])
    ~return_t:frame_t ~category:`Output ~meth:Output.meth
    ~descr:"Output stream to jack."
    (fun p ->
      let source = List.assoc "" p in
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let server = Lang.to_string (List.assoc "server" p) in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      (new output
         ~self_sync ~infallible ~register_telnet ~on_start ~on_stop ~server
         source
        :> Output.output))
