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

module type Generator_t =
sig
  type t
  val length : t -> int (* ticks *)
  val remaining : t -> int (* ticks *)
  val clear : t -> unit
  val fill_frame : t -> Frame.t -> unit
  val add_metadata : t -> int*Frame.metadata -> unit
end

module Make (Generator:Generator_t) =
struct

(* Reads data from an audio buffer generator. The generator can be feeded
 * in parallel, using [lock] if not in the main thread.
 * Store [bufferize] seconds before declaring itself as ready. *)
class virtual source ?(metadata=None) 
                     ~bufferize ~empty_on_abort abg =
  let bufferize = Fmt.ticks_of_seconds bufferize in
  let () = 
    match metadata with
      | None -> ()
      | Some x -> Generator.add_metadata abg x
  in
object (self)

  (** This allows heriting classes to access the generator. *)
  val abg = abg

  val mutable buffering = true
  val lock = Mutex.create ()

  val mutable should_fail = false

  method abort_track = should_fail <- true

  method private length =
    Mutex.lock lock ;
    let r = Generator.length abg in
      Mutex.unlock lock ;
      r

  method is_ready =
    let r = self#length in
      if buffering then
        r > bufferize
      else
        r > 0

  method remaining =
    if should_fail then 0 else
      begin
        let r = self#length in
        Mutex.lock lock ;
        let l = Generator.remaining abg in
        Mutex.unlock lock ;
        if buffering && r <= bufferize then 0 else l
      end

  method private get_frame ab =
    buffering <- false ;
    if should_fail then begin
      should_fail <- false ;
      (* empty the buffer on skip? *)
      if empty_on_abort then
        Generator.clear abg;
      Frame.add_break ab (Frame.position ab)
    end else begin
        Mutex.lock lock ;
        Generator.fill_frame abg ab ;
        if Generator.length abg = 0 then
            buffering <- true;
        Mutex.unlock lock
    end

end

(* Reads data from a fixed audio buffer generator,
 * assuming that it won't be feeded more after instantiation. *)
class consumer ?(metadata=None) abg =
object
  inherit Source.source
  inherit source abg ~metadata ~bufferize:0. ~empty_on_abort:true
  method stype = Source.Fallible
end

end

module From_Float_pcm_Generator =
  Make(struct
         type t = Float_pcm.Generator.t
         let length x = Fmt.ticks_of_samples (Float_pcm.Generator.length x)
         let remaining x = Float_pcm.Generator.remaining x
         let clear = Float_pcm.Generator.clear
         let add_metadata = Float_pcm.Generator.add_metadata
         let fill_frame = AFrame.fill_frame
       end)
