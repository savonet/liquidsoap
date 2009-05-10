(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

 (** Producer/consumer source utils. *)

class virtual ['a] base ~nb_blocks ~blank 
                        ~blocking () = 
  let () = 
    if nb_blocks < 2 then
      raise (Lang.Invalid_value
                 (Lang.int nb_blocks,
                  "Buffered I/O requires a buffer length >= 2."))  
  in
object (self)

  val buffer = Array.init nb_blocks (fun _ -> blank ())
  val mutable read = 0
  val mutable write = 0
  (* Read and write are stored modulo 2*nb_blocks,
   * because we must be able to distinguish the case where the sched is late
   * from the one where the capture is late.
   * And we don't need more than modulo 2*nb_blocks. *)

  initializer
    if blocking then
      (Dtools.Conf.as_bool (Configure.conf#path ["root";"sync"]))#set false

  val wait_m = Mutex.create ()
  val wait_c = Condition.create ()

  val mutable sleep = false
  method sourcering_stop = 
    sleep <- true ;
    Condition.broadcast wait_c
end

class virtual ['a] input ~nb_blocks ~blank 
                         ?(blocking=false) () =
object (self)
  inherit ['a] base ~nb_blocks ~blank 
                    ~blocking ()

  method virtual pull_block : 'a -> unit

  method virtual id : string

  method virtual close : unit

  method sleep = self#sourcering_stop

  method output_get_ready =
    sleep <- false ;
    read <- 0 ; write <- 0 ;
    ignore (Tutils.create
         (fun _ -> self#writer) () self#id) ;

  method private writer =
      let fill () =
         self#pull_block buffer.(write mod nb_blocks);
         write <- (write + 1) mod (2*nb_blocks)
      in
      try
        (* Filling loop *)
        while not sleep do
          if read <> write &&
             write mod nb_blocks = read mod nb_blocks then begin
               (* Wait for the reader to read the block we fancy *)
               Mutex.lock wait_m ;
               if not sleep && read <> write &&
                 write mod nb_blocks = read mod nb_blocks then
                   Condition.wait wait_c wait_m ;
               Mutex.unlock wait_m ;
          end ;
          if not sleep then
            fill () ;
          Condition.signal wait_c
        done ;
        self#close
      with
        | e -> 
           self#close ; 
           self#sourcering_stop ;
           raise e

  method private get_block = 
    (* Check that the writer still has an advance.
     * Otherwise play blank for waiting.. *)
    if write = read then
      begin
        Mutex.lock wait_m ;
        if not sleep && write = read then
          Condition.wait wait_c wait_m;
        Mutex.unlock wait_m 
      end ;
    let b = buffer.(read mod nb_blocks) in
    read <- (read + 1) mod (2*nb_blocks) ;
    Condition.signal wait_c ;
    b
end

class virtual ['a] output ~nb_blocks ~blank 
                          ?(blocking=false) () =
object (self)
  inherit ['a] base ~nb_blocks ~blank ~blocking ()

  method virtual id : string

  method virtual push_block : 'a -> unit

  method virtual close :  unit

  method output_stop = self#sourcering_stop

  method output_start =
    sleep <- false ;
    read <- 0 ; write <- 0 ;
    ignore (Tutils.create (fun () -> self#reader) () self#id)

  method reader =
    try
      (* The output loop *)
      while not sleep do
        if read = write then
        begin
          Mutex.lock wait_m ;
          if not sleep && read = write then
             Condition.wait wait_c wait_m ;
           Mutex.unlock wait_m 
        end ;
        if not sleep then
        begin
          self#push_block buffer.(read mod nb_blocks);
          read <- (read + 1) mod (2*nb_blocks)
        end ;
        Condition.signal wait_c
      done ;
      self#close
    with
      | e -> 
         self#close; 
         self#sourcering_stop ;
         raise e

  method put_block (f:'a -> unit) =
    if read <> write &&
       write mod nb_blocks = read mod nb_blocks then
       begin
         Mutex.lock wait_m ;
         if not sleep && read <> write &&
           write mod nb_blocks = read mod nb_blocks then
             Condition.wait wait_c wait_m ;
         Mutex.unlock wait_m
       end ;
    f buffer.(write mod nb_blocks) ;
    write <- (write + 1) mod (2*nb_blocks) ;
    Condition.signal wait_c
end
