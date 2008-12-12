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

open Source

module Generator = Float_pcm.Generator
module Generated = Generated.From_Float_pcm_Generator

(* Delayed source *)

class lag (source:source) delay =
  let past_len = Fmt.samples_of_seconds delay in
object (self)
  inherit Source.source
  inherit Generated.source
            (Generator.create ())
            ~empty_on_abort:false ~bufferize:delay as generated

  val mutable is_streaming = false
  val mutable delta = 0

  val past = Frame.make ()

  method stype = Source.Infallible 

  method remaining =  
    if is_streaming then
      generated#remaining
    else
      -1

  method is_ready = true

  method get_ready = source#get_ready

  method abort_track = 
    self#log#f 2 "Track skip in delayed stream will be delayed !";
    source#abort_track

  method get_frame buf =
    let cur_pos = AFrame.position buf in
    let taken = 
      if Generator.length abg >= past_len then
        begin
          if not is_streaming then
          (* Switching back to streaming:
           * Adding new track *)
            begin
              is_streaming <- true;
              AFrame.add_break buf cur_pos;
              Generator.remove abg ~initial:false 0;
              0
            end
          else
            begin
              AFrame.fill_frame abg buf;
              (AFrame.position buf) - cur_pos
            end
        end
      else
        begin
          let bsize = AFrame.size buf in
          let remaining = bsize - cur_pos in
          AFrame.blankify buf cur_pos remaining;
          AFrame.add_break buf bsize;
          remaining
       end
    in
    delta <- delta - taken;
    while delta < 0 do
      (* past should always be empty here.. *)
      AFrame.clear past;
      (* Get new data *)
      if source#is_ready then
        begin
          source#get past;
          AFrame.feed_frame abg past;
          delta <- delta + (AFrame.position past)
        end
      else
        begin
          let len = min (AFrame.size past) (-delta) in
          AFrame.blankify past 0 len;
          AFrame.add_break past len;
          AFrame.feed_frame abg past;
          delta <- delta + len
        end
    done
end

let () =
  Lang.add_operator "lag"
    [ "", Lang.float_t, None, Some "Delay in seconds.";
      "", Lang.source_t, None, None ]
    ~category:Lang.SoundProcessing
    ~descr:"Add a constant delay to a stream, \
            filling with blank when not available."
    (fun p ->
       let f n = Lang.assoc "" n p in
       let duration, src =
         Lang.to_float (f 1),
         Lang.to_source (f 2)
       in
         (new lag src duration :>source))
