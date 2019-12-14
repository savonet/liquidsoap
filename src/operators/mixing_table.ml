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

open Source

class mixing ~kind source =
  let n = Array.length source in
  object (self)
    inherit operator ~name:"mix" kind (Array.to_list source)

    initializer assert (n > 0)

    val mutable vol = Array.make n 1.

    val mutable sel = Array.make n false

    val mutable single = Array.make n false

    method stype = Infallible

    method is_ready = true

    method remaining = -1

    method self_sync = Array.exists (fun s -> s#self_sync) source

    val tmp = Frame.create kind

    initializer
    (* Server commands *)
    ns_kind <- "mixer" ;
    self#register_command "skip"
      ~descr:"Skip current track on all enabled sources." (fun a ->
        (source.(int_of_string a))#abort_track ; "OK") ;
    self#register_command "volume" ~descr:"Set volume for a given source."
      ~usage:"volume <source nb> <vol%>" (fun a ->
        if Str.string_match (Str.regexp "\\([0-9]+\\) \\([0-9]+\\)") a 0 then (
          let i = int_of_string (Str.matched_group 1 a) in
          let v = int_of_string (Str.matched_group 2 a) in
          vol.(i) <- float v /. 100. ;
          self#status i )
        else "Usage: volume <source nb> <vol%>") ;
    self#register_command "select" ~descr:"Enable/disable a source."
      ~usage:"select <source nb> <true|false>" (fun a ->
        if Str.string_match (Str.regexp "\\([0-9]+\\) \\(true\\|false\\)") a 0
        then (
          let i = int_of_string (Str.matched_group 1 a) in
          let v = Str.matched_group 2 a in
          sel.(i) <- v = "true" ;
          self#status i )
        else "Usage: select <source nb> <true|false>") ;
    self#register_command "single"
      ~descr:"Enable/disable automatic stop at the end of track."
      ~usage:"single <source nb> <true|false>" (fun a ->
        if Str.string_match (Str.regexp "\\([0-9]+\\) \\(true\\|false\\)") a 0
        then (
          let i = int_of_string (Str.matched_group 1 a) in
          let v = Str.matched_group 2 a in
          single.(i) <- v = "true" ;
          self#status i )
        else "Usage: single <source nb> <true|false>") ;
    self#register_command "status" ~descr:"Display current status." (fun a ->
        self#status (int_of_string a)) ;
    self#register_command "inputs" ~descr:"Print the list of input sources."
      (fun _ -> Array.fold_left (fun e s -> e ^ " " ^ s#id) "" source)

    method private get_frame buf =
      let p = AFrame.position buf in
      let r = AFrame.size () - p in
      AFrame.blankify buf p r ;
      for i = 0 to n - 1 do
        if sel.(i) && (source.(i))#is_ready then (
          AFrame.clear tmp ;
          AFrame.set_breaks tmp [p] ;
          if single.(i) then (
            (source.(i))#get tmp ;
            if AFrame.is_partial tmp then sel.(i) <- false )
          else
            while AFrame.is_partial tmp && (source.(i))#is_ready do
              (source.(i))#get tmp
            done ;
          List.iter
            (fun (t, m) -> AFrame.set_metadata buf t m)
            (AFrame.get_all_metadata tmp) ;
          AFrame.multiply tmp p r vol.(i) ;
          AFrame.add buf p tmp p r )
      done ;
      AFrame.add_break buf (AFrame.size ())

    method abort_track =
      for i = 0 to n - 1 do
        if sel.(i) then (source.(i))#abort_track
      done

    method private status i =
      Printf.sprintf "ready=%b selected=%b single=%b volume=%d%% remaining=%s"
        (source.(i))#is_ready sel.(i) single.(i)
        (int_of_float (vol.(i) *. 100.))
        (let r = (source.(i))#remaining in
         if r = -1 then "(undef)"
         else Printf.sprintf "%.2f" (Frame.seconds_of_master r))
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "mix"
    [("", Lang.list_t (Lang.source_t k), None, None)]
    ~kind:(Lang.Unconstrained k) ~category:Lang.SoundProcessing
    ~descr:"Mixing table controllable via the telnet interface."
    (fun p kind ->
      let sources = Lang.to_source_list (List.assoc "" p) in
      new mixing ~kind (Array.of_list sources))
