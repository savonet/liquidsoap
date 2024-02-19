(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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

(** Output using ao lib. *)

open Ao

(** As with ALSA (even more maybe) it would be better to have one clock
  * per driver... but it might also depend on driver options. *)
let get_clock = Tutils.lazy_cell (fun () -> Clock.clock "ao")

class output ~clock_safe ~nb_blocks ~driver ~register_telnet ~infallible
  ~on_start ~on_stop ~options ?channels_matrix source start =
  let samples_per_frame = AFrame.size () in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let bytes_per_sample = 2 in
  object (self)
    inherit
      Output.output
        ~register_telnet ~infallible ~on_start ~on_stop ~name:"ao"
          ~output_kind:"output.ao" source start as super

    inherit [Bytes.t] IoRing.output ~nb_blocks as ioring

    method! wake_up a =
      super#wake_up a;
      let blank () =
        Bytes.make
          (samples_per_frame * self#audio_channels * bytes_per_sample)
          '0'
      in
      ioring#init blank

    method! private set_clock =
      super#set_clock;
      if clock_safe then
        Clock.unify ~pos:self#pos self#clock
          (Clock.create_known (get_clock () :> Source.clock))

    val mutable device = None
    method! self_sync = (`Dynamic, device <> None)

    method get_device =
      match device with
        | Some d -> d
        | None ->
            (* Wait for things to settle... TODO I don't need that! *)
            Thread.delay (5. *. Lazy.force Frame.duration);
            let driver =
              if driver = "" then get_default_driver () else find_driver driver
            in
            let dev =
              self#log#important "Opening %s (%d channels)..." driver.Ao.name
                self#audio_channels;
              open_live ~driver ~options ?channels_matrix
                ~rate:samples_per_second ~bits:(bytes_per_sample * 8)
                ~channels:self#audio_channels ()
            in
            device <- Some dev;
            dev

    method close =
      match device with
        | Some d ->
            Ao.close d;
            device <- None
        | None -> ()

    method push_block data =
      let dev = self#get_device in
      play dev (Bytes.unsafe_to_string data)

    method send_frame wav =
      if not (Frame.is_partial wav) then (
        let push data =
          let pcm = AFrame.pcm wav in
          assert (Array.length pcm = self#audio_channels);
          Audio.S16LE.of_audio pcm 0 data 0 (Audio.length pcm)
        in
        ioring#put_block push)

    method! reset = ()
  end

let _ =
  let return_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:Modules.output "ao"
    (Output.proto
    @ [
        ( "clock_safe",
          Lang.bool_t,
          Some (Lang.bool true),
          Some "Use the dedicated AO clock." );
        ( "driver",
          Lang.string_t,
          Some (Lang.string ""),
          Some "Driver to be used, \"\" for AO's default." );
        ( "channels_matrix",
          Lang.string_t,
          Some (Lang.string ""),
          Some "Output channels matrix, \"\" for AO's default." );
        ( "buffer_size",
          Lang.int_t,
          Some (Lang.int 2),
          Some "Set buffer size, in frames." );
        ( "options",
          Lang.metadata_t,
          Some (Lang.list []),
          Some "List of parameters, depends on the driver." );
        ("", Lang.source_t return_t, None, None);
      ])
    ~category:`Output ~meth:Output.meth
    ~descr:"Output stream to local sound card using libao." ~return_t
    (fun p ->
      let clock_safe = Lang.to_bool (List.assoc "clock_safe" p) in
      let driver = Lang.to_string (List.assoc "driver" p) in
      let nb_blocks = Lang.to_int (List.assoc "buffer_size" p) in
      let options =
        List.map
          (fun x ->
            let a, b = Lang.to_product x in
            (Lang.to_string a, Lang.to_string b))
          (Lang.to_list (List.assoc "options" p))
      in
      let channels_matrix = Lang.to_string (List.assoc "channels_matrix" p) in
      let channels_matrix =
        if channels_matrix = "" then None else Some channels_matrix
      in
      let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
      let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply f [])
      in
      let source = List.assoc "" p in
      (new output
         ~clock_safe ~nb_blocks ~driver ~infallible ~register_telnet ~on_start
         ~on_stop ?channels_matrix ~options source start
        :> Output.output))
