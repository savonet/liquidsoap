(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(** Creates an autonomous local clock.
  * As an operator it is an identity, but it triggers the creation of a
  * clock that will animate independently the subgraph.
  * This should usually be used in conjunction with a buffer interfacing
  * this clock with another. *)
class clock ~kind source =
object (self)
  inherit Id.id ~name:"clock" ~kind source
  method set_clock =
    let my_clock = new Clock.wallclock ~sync:true self#id in
    let my_clock = Clock.create_known my_clock in
      Clock.unify source#clock my_clock ;
      Clock.unify self#clock my_clock
end

(** Create a buffer between two clocks.
  *
  * This creates an active operator in the inner clock (the action consists
  * in filling the buffer) but it does not create or force in any
  * way the clock that's going to animate it.
  *
  * We actually create two sources, to avoid the mess of having a same
  * source belonging to one clock but being animated by another.
  * This makes it possible to have the inner clock stop and shutdown
  * the source that feeds the buffer, without disturbing the other clock
  * in which the buffer-consumer will still behave OK (except obviously
  * that the buffer will empty). *)
module Buffer =
struct

  module Generator = Generator.From_frames

  (* The kind of value shared by a producer and a consumer. *)
  type control = {
    lock : Mutex.t ;
    generator : Generator.t ;
    mutable buffering : bool ;
    mutable abort : bool
  }

  let proceed control f =
    Tutils.mutexify control.lock f ()

  (** The source which produces data by reading the buffer. *)
  class producer ~kind source c =
  object (self)
    inherit Source.source kind

    method stype = Source.Fallible

    method remaining =
      (* This is not obvious, notably it is never infinite (-1),
       * but that's the same "choice" as with input.http/harbor(). *)
      proceed c (fun () -> Generator.remaining c.generator)

    method is_ready =
      proceed c (fun () -> not c.buffering)

    method private get_frame frame =
      proceed c
        (fun () ->
           assert (not c.buffering) ;
           Generator.fill c.generator frame ;
           if Generator.length c.generator = 0 then begin
             self#log#f 3 "Buffer emptied, start buffering..." ;
             c.buffering <- true
           end)

    method abort_track =
      proceed c (fun () -> c.abort <- true)

  end

  class consumer
    ~autostart ~infallible ~on_start ~on_stop ~pre_buffer ~max_buffer
    ~kind source_val c
  =
    let prebuf = Frame.master_of_seconds pre_buffer in
    let maxbuf = Frame.master_of_seconds max_buffer in
  object (self)
    inherit Output.output
      ~output_kind:"buffer" ~content_kind:kind
      ~infallible ~on_start ~on_stop
      source_val
      autostart

    method output_reset = ()
    method output_start = ()
    method output_stop  = ()

    val source = Lang.to_source source_val

    method output_send frame =
      proceed c
        (fun () ->
           if c.abort then begin
             c.abort <- false ;
             source#abort_track
           end ;
           Generator.feed_from_frame c.generator frame ;
           if Generator.length c.generator > prebuf then begin
             c.buffering <- false ;
             if Generator.length c.generator > maxbuf then
               Generator.remove c.generator
                 (Generator.length c.generator - maxbuf)
           end)

  end

  let create ~autostart ~infallible ~on_start ~on_stop
        ~pre_buffer ~max_buffer ~kind source_val =
    let control = {
      generator = Generator.create () ;
      lock = Mutex.create () ;
      buffering = true ;
      abort = false
    } in
    let source = Lang.to_source source_val in
    let _ =
      new consumer
        ~autostart ~infallible ~on_start ~on_stop
        ~kind source_val
        ~pre_buffer ~max_buffer control
    in
      new producer ~kind source control

end

let () =
  let k = Lang.univ_t 1 in
    Lang.add_operator "clock"
      ["", Lang.source_t k, None, None]
      ~kind:(Lang.Unconstrained k)
      ~category:Lang.Output (* Whatever... TODO create categories *)
      ~descr:"Forces a source and other time-dependent sources to belong \
              to a new clock that runs indepently of others."
      (fun p kind ->
         let s = List.assoc "" p in
         let src = Lang.to_source s in
           new clock ~kind src)

let () =
  let k = Lang.univ_t 1 in
    Lang.add_operator "buffer"
      (Output.proto @
       ["buffer", Lang.float_t, Some (Lang.float 1.),
          Some "Amount of data to pre-buffer, in seconds." ;
        "max", Lang.float_t, Some (Lang.float 10.),
          Some "Maximum amount of buffered data, in seconds." ;
        "", Lang.source_t k, None, None])
      ~kind:(Lang.Unconstrained k)
      ~category:Lang.Output (* Whatever... TODO create categories *)
      ~descr:"Create a buffer between two different clocks."
      (fun p kind ->
         let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
         let autostart = Lang.to_bool (List.assoc "start" p) in
         let on_start = List.assoc "on_start" p in
         let on_stop = List.assoc "on_stop" p in
         let on_start () = ignore (Lang.apply ~t:Lang.unit_t on_start []) in
         let on_stop () = ignore (Lang.apply ~t:Lang.unit_t on_stop []) in
         let s = List.assoc "" p in
         let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
         let max_buffer = Lang.to_float (List.assoc "max" p) in
           if pre_buffer >= max_buffer then
             raise (Lang.Invalid_value
                      (List.assoc "max" p,
                       "Maximum buffering inferior to pre-buffering"));
           Buffer.create
             ~infallible ~autostart ~on_start ~on_stop
             ~pre_buffer ~max_buffer ~kind s)
