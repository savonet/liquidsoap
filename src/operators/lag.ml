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
  inherit operator [source] as super

  val abg = Generator.create ()
  val past = Frame.make ()
  val mutable is_streaming = false

  method stype = Source.Infallible 

  method remaining = 
    if is_streaming then
      Generator.remaining abg + source#remaining
    else
      -1

  method is_ready = true

  method abort_track = 
    self#log#f 2 "Track skip in delayed stream will be delayed !";
    source#abort_track

  method get_frame buf =
    (* past should always be empty here.. *)
    AFrame.advance past;
    if Generator.length abg < past_len then
     begin 
      (* Get new data *)
      while Frame.is_partial past do
        source#get past
      done;
      Generator.feed_from_frame abg past
     end;
   begin
    if Generator.length abg >= past_len then
      if not is_streaming then
       begin
        self#log#f 3 "Switching to buffered data.";
        is_streaming <- true;
       end
   end;
   assert(Generator.length abg < past_len || is_streaming);
   if is_streaming then
      Generator.fill abg buf
   else
     begin
      let pos = AFrame.position buf in
      let len = AFrame.size buf in
      AFrame.blankify buf pos (len-pos);
      AFrame.add_break buf len
     end
end

let () =
  Lang.add_operator "lag"
    [ "", Lang.float_t, None, Some "Delay in seconds.";
      "", Lang.source_t, None, Some "Original source. Must be infaillible." ]
    ~category:Lang.SoundProcessing
    ~descr:"Add a constant delay to a stream, \
            filling with blank when not available. \
            This operator should be used just before the \
            final output."
    (fun p ->
       let f n = Lang.assoc "" n p in
       let duration, src =
         Lang.to_float (f 1),
         Lang.to_source (f 2)
       in
       if src#stype <> Infallible then
         raise (Lang.Invalid_value (f 2,"That source should be infallible.")) ;
       (new lag src duration :>source))
