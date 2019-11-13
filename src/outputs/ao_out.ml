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

(** Output using ao lib. *)

open Ao

(** As with ALSA (even more maybe) it would be better to have one clock
  * per driver... but it might also depend on driver options. *)
let get_clock = Tutils.lazy_cell (fun () -> new Clock.clock "ao")

class output ~kind ~clock_safe ~nb_blocks ~driver ~infallible ~on_start
  ~on_stop ~options ?channels_matrix source start =
  let channels = (Frame.type_of_kind kind).Frame.audio in
  let samples_per_frame = AFrame.size () in
  let samples_per_second = Lazy.force Frame.audio_rate in
  let bytes_per_sample = 2 in
  let blank () =
    Bytes.make (samples_per_frame * channels * bytes_per_sample) '0'
  in
  object (self)
    inherit
      Output.output
        ~content_kind:kind ~infallible ~on_start ~on_stop ~name:"ao"
          ~output_kind:"output.ao" source start as super

    inherit [Bytes.t] IoRing.output ~nb_blocks ~blank as ioring

    method private set_clock =
      super#set_clock ;
      if clock_safe then
        Clock.unify self#clock
          (Clock.create_known (get_clock () :> Clock.clock))

    val mutable device = None

    method self_sync = device <> None

    method get_device =
      match device with
        | Some d ->
            d
        | None ->
            (* Wait for things to settle... TODO I don't need that! *)
            Thread.delay (5. *. Lazy.force Frame.duration) ;
            let driver =
              if driver = "" then get_default_driver () else find_driver driver
            in
            let dev =
              (self#log)#important "Opening %s (%d channels)..." driver.Ao.name
                channels ;
              open_live ~driver ~options ?channels_matrix
                ~rate:samples_per_second ~bits:(bytes_per_sample * 8) ~channels
                ()
            in
            device <- Some dev ;
            dev

    method close =
      match device with
        | Some d ->
            Ao.close d ;
            device <- None
        | None ->
            ()

    method push_block data =
      let dev = self#get_device in
      play dev (Bytes.unsafe_to_string data)

    method output_send wav =
      if not (Frame.is_partial wav) then (
        let push data =
          let pcm = AFrame.content wav 0 in
          assert (Array.length pcm = channels) ;
          Audio.S16LE.of_audio pcm data 0
        in
        ioring#put_block push )

    method output_reset = ()
  end

let () =
  let kind = Lang.any_fixed_with ~audio:1 () in
  let kind = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "output.ao" ~active:true
    ( Output.proto
    @ [ ( "clock_safe",
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
          Some (Lang.list ~t:(Lang.product_t Lang.string_t Lang.string_t) []),
          Some "List of parameters, depends on the driver." );
        ("", Lang.source_t kind, None, None) ] )
    ~category:Lang.Output
    ~descr:"Output stream to local sound card using libao."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
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
      let start = Lang.to_bool (List.assoc "start" p) in
      let on_start =
        let f = List.assoc "on_start" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let on_stop =
        let f = List.assoc "on_stop" p in
        fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
      in
      let source = List.assoc "" p in
      ( new output
          ~kind ~clock_safe ~nb_blocks ~driver ~infallible ~on_start ~on_stop
          ?channels_matrix ~options source start
        :> Source.source ))
