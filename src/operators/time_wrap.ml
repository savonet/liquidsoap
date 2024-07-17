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

(** In theory this operator only makes sense with huge restrictions on it:
  *  in a sense there shouldn't even be a single active operator in its
  *  sources' scope. *)
class wrap (source:source) =
  let time = Unix.gettimeofday in
  let usleep d =
    (* In some implementations,
     * Thread.delay uses Unix.select which can raise EINTR.
     * A really good implementation would keep track of the elapsed time
     * and then trigger another Thread.delay for the remaining time.
     * This cheap thing does the job for now.. *)
    try Thread.delay d with Unix.Unix_error (Unix.EINTR,_,_) -> ()
  in
  let acc = ref 0 in
  let sync = true in
  let t0 = ref 0. in
  let ticks = ref 0L in
  let delay () =
    !t0 +.
    Fmt.seconds_per_frame () *. (Int64.to_float (Int64.add !ticks 1L)) -.
    time ()
  in
  let last_latency_log = ref (time ()) in
  let inputs,outputs = ref 0, ref 0 in
object (self)
  (* Do not inherit from active_operator: we'll be secretly active instead. *)
  (* TODO hide our son [source] so that the main root doesn't take it *)
  inherit active_operator source as super

  method stype = Infallible
  method is_ready = true
  method remaining = source#remaining
  method abort_track = source#abort_track

  (* The buffer generator will be filled from the pseudo-root thread.
   * TODO: enhance generators to support breaks and metadata. *)
  val generator = Generator.create ()

  val mutable catching_up = false

  method get_frame ab =
    (* self#log#f 3
      "%d-%d:: Frame: %d/%d || Gen: %d"
      !inputs !outputs (AFrame.position ab) (AFrame.size ab)
      (Generator.length generator) ; *)
    if catching_up then
      if Generator.length generator > 200 * AFrame.size ab then
        catching_up <- false ;
    if catching_up ||
       Generator.length generator < AFrame.size ab - AFrame.position ab
    then
      begin
        if catching_up = false then begin
          self#log#f 2 "I'm late!" ;
          catching_up <- true
        end ;
        Float_pcm.blankify
          (AFrame.get_float_pcm ab) (AFrame.position ab) (AFrame.size ab) ;
        AFrame.add_break ab (AFrame.size ab)
      end
    else
      begin
        AFrame.fill_frame generator ab ;
        incr outputs ;
        assert (not (AFrame.is_partial ab))
     end

  method output = ()
  method output_reset = ()
  method output_get_ready =
    (* Do the most to avoid making a ready output.alsa() wait. *)
    Duppy.Task.add Tutils.scheduler 
      { Duppy.Task.priority = Tutils.Blocking ;
        (* Priority should not really be blocking, but rather a dedicated one
        * associated to a dedicated queue... we'll think about that when
        * time warp actually becomes possible :p *)
        Duppy.Task.events   = [`Delay 0.] ;
        Duppy.Task.handler  = (fun _ ->
           ignore (Tutils.create (fun () -> self#animate) () "Time wrap root") ;
           []) }

  (* This code comes from src/root.ml. In the future it is possible that root
   * becomes an operator (which this one would extend) to avoid the code
   * duplication. *)
  method animate =
    let max_latency = -. Root.conf_max_latency#get in
    t0 := time () ;
    while true do
      let rem = if not sync then 0. else delay () in
      (* One new frame each time because the generator doesn't copy. *)
      let frame = Fmt.create_frame () in
        (* Sleep a while or worry about the latency *)
        if (not sync) || rem > 0. then begin
          acc := 0 ;
          usleep rem
        end else begin
          incr acc ;
          if rem < max_latency then begin
            self#log#f 2 "Too much latency! Resetting active sources.." ;
            (* iter (fun s -> s#output_reset) ; *)
            t0 := time () ;
            ticks := 0L ;
            acc := 0
          end else if
            (rem <= -1. || !acc >= 100) && !last_latency_log +. 1. < time ()
          then begin
            last_latency_log := time () ;
            self#log#f 2 "We must catchup %.2f seconds%s!"
              (-. rem)
              (if !acc <= 100 then "" else
                 " (we've been late for 100 rounds)") ;
            acc := 0
          end
        end ;
        ticks := Int64.add !ticks 1L ;
        (* Pump data out of the source. *)
        AFrame.advance frame ;
        while AFrame.is_partial frame do
          source#get frame
        done ;
        incr inputs ;
        Generator.feed generator (AFrame.get_float_pcm frame)
    done

end

let () =
  Lang.add_operator "time_wrap"
    ["", Lang.source_t, None, None]
    ~category:Lang.Output
    ~flags:[Lang.Experimental;Lang.Hidden]
    ~descr:"Separates the computation of a sub-source from the main \
            thread, creating an asynchronous interface between them."
    (fun p ->
       let s = List.assoc "" p in
       let src = Lang.to_source s in
         if src#stype <> Infallible then
           raise (Lang.Invalid_value (s,"That source should be infallible.")) ;
         ((new wrap src):>source))
