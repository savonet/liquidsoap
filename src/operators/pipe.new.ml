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

type p = in_channel*out_channel
type t = Undef | Mono of p*p | Stereo of p

class pipe ~stereo ~headers command source =
object (self)
  inherit Types.operator [source]

  (* I assume that the command won't fail. *)
  method stype = source#stype
  method remaining = source#remaining
  method abort_track = source#abort_track
  method is_ready = source#is_ready

  val mutable pipes : t = Undef

  method private wake_up l =
    source#get_ready ((self:>Types.source)::l) ;
    begin if stereo then
      let i,o = Unix.open_process command in
        if headers then
          output_string o (Wav.header { Mixer.Buffer.format with Mixer.channels = 1 }) ;
        pipes <- Stereo (i,o) ;
        flush o ;
        Wav.skip_header i
    else
      let li,lo = Unix.open_process command in
      let ri,ro = Unix.open_process command in
        if headers then begin
          let h = Wav.header { Mixer.Buffer.format with Mixer.channels = 1 } in
            output_string lo h ;
            output_string ro h ;
        end ;
        pipes <- Mono ((li,lo),(ri,ro)) ;
        flush lo ;
        flush ro ;
        Wav.skip_header li ;
        Wav.skip_header ri
    end ;

  method private sleep =
    source#leave (self:>Types.source) ;
    match pipes with
      | Undef -> assert false
      | Stereo p ->
          ignore (Unix.close_process p)
      | Mono (p1,p2) ->
          ignore (Unix.close_process p1) ;
          ignore (Unix.close_process p2)

  method get_frame ab =
    let p1 = Mixer.Buffer.position ab in
    let p2 = source#get ab ; Mixer.Buffer.position ab in
    let s = Mixer.Buffer.to_string ab in
      match pipes with
        | Stereo (i,o) ->
            output o s p1 (p2-p1) ;
            flush o ;
            for p = p1 to p2-1 do
              s.[p] <- input_char i
            done
        | Mono ((li,lo),(ri,ro)) ->
            assert ((p2-p1) mod 4 = 0) ;
            let n = (p2-p1)/4 in
              for i = 0 to n-1 do
                output_char lo s.[p1+i*4+0] ;
                output_char lo s.[p1+i*4+1] ;
                output_char ro s.[p1+i*4+2] ;
                output_char ro s.[p1+i*4+3] ;
              done ;
              flush lo ;
              flush ro ;
              for i = 0 to n-1 do
                Printf.printf "plouf %d\n%!" n ;
                s.[p1+i*4+0] <- input_char li ;
                Printf.printf "plouf %d\n%!" n ;
                s.[p1+i*4+1] <- input_char li ;
                Printf.printf "plouf %d\n%!" n ;
                s.[p1+i*4+2] <- input_char ri ;
                Printf.printf "plouf %d\n%!" n ;
                s.[p1+i*4+3] <- input_char ri ;
                Printf.printf "plouf %d\n%!" n ;
              done
        | Undef -> assert false

end

let _ =
  Lang.add_operator "pipe"
    ~descr:("Filter data through an external process. The process should read"^
            " raw CD format on stdin and write the same on stdout. The "^
            "command should output one frame for every input frame, and flush"^
            " its output. Otherwise, liquidsoap will block.")
    [ "stereo", Lang.bool_t, Some (Lang.bool true),
      Some ("True if the command can process stereo. Otherwise, two instances"^
            " of the command are launched, one for each channel.") ;
      "headers", Lang.bool_t, Some (Lang.bool false),
      Some "True if WAV headers should be used. False if data is raw." ;
      "", Lang.string_t, None, None ;
      "", Lang.source_t, None, None ]
    (fun p ->
       let c = Lang.to_string (Lang.assoc "" 1 p) in
       let s = Lang.to_source (Lang.assoc "" 2 p) in
       let headers = Lang.to_bool (List.assoc "headers" p) in
       let stereo = Lang.to_bool (List.assoc "stereo" p) in
         ((new pipe ~headers ~stereo c s):>Types.source))
