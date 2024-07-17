(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

open Types

class mixing source =
  let n = Array.length source in
object (self)
  inherit operator (Array.to_list source) as super

  initializer assert (n>0)

  val mutable vol = Array.create n 1.
  val mutable sel = Array.create n false
  val mutable single = Array.create n false

  method stype = Infallible
  method is_ready = true

  val tmp = Mixer.Buffer.create ()

  method get_frame buf =
    let p = Mixer.Buffer.position buf in
    let r = Mixer.Buffer.size - p in
      Mixer.Buffer.blankify buf p (Mixer.Buffer.size-p) ;
      for i = 0 to n-1 do
        if sel.(i) && source.(i)#is_ready then begin
          Mixer.Buffer.free tmp ;
          Mixer.Buffer.set_breaks tmp [p] ;
          let b = ref [] in
            if single.(i) then
              ( source.(i)#get tmp ;
                if Mixer.Buffer.is_partial tmp then sel.(i) <- false )
            else
              while Mixer.Buffer.is_partial tmp &&
                    !b <> Mixer.Buffer.breaks tmp &&
                    not (Mixer.Buffer.ill ~base:p tmp) do
                b := Mixer.Buffer.breaks tmp ;
                source.(i)#get tmp
              done ;
            List.iter
              (fun (t,m) ->
                 Mixer.Buffer.set_metadata buf t m)
              (Mixer.Buffer.get_all_metadata tmp) ;
            Mixer.Buffer.change_volume tmp p r vol.(i) ;
            Mixer.Buffer.add buf p tmp p r
        end
      done ;
      Mixer.Buffer.add_break buf Mixer.Buffer.size

  method abort_track = ()

  method status i =
    Printf.sprintf
      "ready=%b selected=%b single=%b volume=%d%% remaining=%s"
      source.(i)#is_ready
      sel.(i)
      single.(i)
      (int_of_float (vol.(i)*.100.))
      (let r = source.(i)#remaining in
         if r = -1 then "(undef)" else
           Printf.sprintf "%.2fsec" ((float r)*.Mixer.Buffer.length))

  val mutable ns = []
  method wake_up activation =
    super#wake_up activation ;
    (* Server commands *)
    if ns = [] then ns <- Server.register [self#id] "mixer" ;
    Server.add ~ns "skip"
      (fun a ->
         source.(int_of_string a)#abort_track ;
         "OK") ;
    Server.add ~ns "volume"
      (fun a ->
         if Str.string_match (Str.regexp "\\([0-9]+\\) \\([0-9]+\\)") a 0 then
           let i = int_of_string (Str.matched_group 1 a) in
           let v = int_of_string (Str.matched_group 2 a) in
             vol.(i) <- (float v)/.100. ;
             self#status i
         else
           "Usage: vol [source nb] [vol%]") ;
    Server.add ~ns "select"
      (fun a ->
         if Str.string_match
              (Str.regexp "\\([0-9]+\\) \\(true\\|false\\)") a 0 then
           let i = int_of_string (Str.matched_group 1 a) in
           let v = Str.matched_group 2 a in
             sel.(i) <- v = "true" ;
             self#status i
         else
           "Usage: select [source nb] [true|false]") ;
    Server.add ~ns "single"
      (fun a ->
         if Str.string_match
              (Str.regexp "\\([0-9]+\\) \\(true\\|false\\)") a 0 then
           let i = int_of_string (Str.matched_group 1 a) in
           let v = Str.matched_group 2 a in
             single.(i) <- v = "true" ;
             self#status i
         else
           "Usage: single [source nb] [true|false]") ;
    Server.add ~ns "status"
      (fun a -> self#status (int_of_string a)) ;
    Server.add ~ns "inputs"
      (fun _ -> Array.fold_left (fun e s -> e^" "^s#id) "" source)

end

let _ =
  Lang.add_operator "mix"
    [ "", Lang.list_t Lang.source_t, None, None ]
    ~descr:"Mixing table controllable via the telnet interface."
    (fun p ->
       let sources = Lang.to_source_list (List.assoc "" p) in
         ((new mixing (Array.of_list sources)):>source))
