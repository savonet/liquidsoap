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

(** Output using ao lib. *)

open Ao

module SyncSource = Clock.MkSyncSource (struct
  type t = unit

  let to_string _ = "ao"
end)

let sync_source = SyncSource.make ()

class output ~self_sync ~driver ~register_telnet ~infallible ~options
  ?channels_matrix source start =
  let samples_per_second = Lazy.force Frame.audio_rate in
  let bytes_per_sample = 2 in
  object (self)
    inherit
      Output.output
        ~register_telnet ~infallible ~name:"ao" ~output_kind:"output.ao" source
          start

    val mutable device = None

    method self_sync =
      if self_sync then
        (`Dynamic, if device <> None then Some sync_source else None)
      else (`Static, None)

    method private get_device =
      match device with
        | Some d -> d
        | None ->
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

    method start = ignore self#get_device

    method stop =
      match device with
        | Some d ->
            Ao.close d;
            device <- None
        | None -> ()

    method send_frame frame = play self#get_device (AFrame.s16le frame)
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
        ( "self_sync",
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
        ( "options",
          Lang.metadata_t,
          Some (Lang.list []),
          Some "List of parameters, depends on the driver." );
        ("", Lang.source_t return_t, None, None);
      ])
    ~category:`Output ~meth:(Start_stop.meth ())
    ~callbacks:(Start_stop.callbacks ~label:"output")
    ~descr:"Output stream to local sound card using libao." ~return_t
    (fun p ->
      let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
      let driver = Lang.to_string (List.assoc "driver" p) in
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
      let source = List.assoc "" p in
      (new output
         ~self_sync ~driver ~infallible ~register_telnet ?channels_matrix
         ~options source start
        :> Output.output))
