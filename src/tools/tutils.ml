(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

open Dtools

let mutexify lock f =
  fun x ->
    Mutex.lock lock ;
    try
      let ans = f x in Mutex.unlock lock ; ans
    with
      | e -> Mutex.unlock lock ; raise e

(** Manage a set of threads and make sure they terminate correctly,
  * i.e. not by raising an exception. *)

let lock = Mutex.create ()
let uncaught = ref None
module Set = Set.Make (struct
                         type t = (string*Thread.t)
                         let compare = compare
                       end)
let all = ref (Set.empty)

(** Check that thread [s] is still running. *)
let running s id = Set.mem (s,id) !all

let join_all () =
  try
    while true do
      let id =
        Mutex.lock lock ;
        snd (Set.choose !all)
      in
        Mutex.unlock lock ;
        Thread.join id
    done
  with
    | Not_found -> Mutex.unlock lock

let no_problem =
  let m = Mutex.create () in
    Mutex.lock m ;
    m

let log = Log.make ["threads"]

let create f x s =
  mutexify lock (
    fun () ->
      let id =
        Thread.create
          (fun x ->
             try
               f x ;
               Mutex.lock lock ; 
               all := Set.remove (s,(Thread.self ())) !all ;
               log#f 3 "thread %S exited (%d remaining)" s (Set.cardinal !all) ;
               Mutex.unlock lock
             with e ->
               Mutex.lock lock ; 
               begin match e with
                 | Failure e ->
                     log#f 1 "thread %S failed: %s" s e
                 | e ->
                     log#f 1 "thread %S aborts with exception %s !"
                              s (Printexc.to_string e)
               end ;
               all := Set.remove (s,(Thread.self ())) !all ;
               uncaught := Some e ;
               Mutex.unlock no_problem ;
               Mutex.unlock lock ;
               raise e)
          x
      in
        all := Set.add (s,id) !all ;
        log#f 3 "Created thread %S (%d total)" s (Set.cardinal !all) ;
	id
  ) ()

module Task =
struct

  type task     = unit -> return_t
  and  return_t = Sleep of task | Yield of task | Finish
  type id = return_t ref

  let lock = Mutex.create ()
  let global_sleep = Mutex.create ()
  let sleeping = ref []
  let active = Queue.create ()

  let add t =
    Mutex.lock lock ;
    let global_wake_up = Queue.is_empty active in
      Queue.add t active ;
      if global_wake_up then begin
        log#f 4 "Scheduler wakes up" ;
        Mutex.unlock global_sleep
      end ;
      Mutex.unlock lock

  let wake_up t =
    Mutex.lock lock ;
    match !t with
      | Sleep k ->
          t := Yield k ;
          let b,l = Utils.filter_exists (fun x -> x==t) !sleeping in
            sleeping := l ;
            Mutex.unlock lock ;
            if b then add t
      | _ -> Mutex.unlock lock

  let create_thread = create

  (** [create ~k task] creates a new task running [task] and gives back the id
    * of that task by calling [k t] before that the task is ran. *)
  let create ?(k=ignore) task =
    let t = ref (Yield task) in
      k t ;
      (* We must call k before adding t because the task could start running
       * before that add returns. *)
      add t

  let rec scheduler () =
    Mutex.lock global_sleep ;
    begin try
      Mutex.lock lock ;
      let t = Queue.pop active in
        begin match !t with
          | Sleep k -> sleeping := t::!sleeping ; Mutex.unlock lock
          | Finish -> Mutex.unlock lock
          | Yield k ->
              Mutex.unlock lock ;
              begin try t := k () with
                | e ->
                    uncaught := Some e ;
                    log#f 1 "Task aborts with exception %s"
                             (Printexc.to_string e) ;
                    Mutex.unlock no_problem
              end ;
              add t
        end ;
        Mutex.unlock global_sleep
    with
      | Queue.Empty -> Mutex.unlock lock ; log#f 4 "Scheduler sleeps"
    end ;
    scheduler ()

  let start () =
     Mutex.lock global_sleep ;
     ignore (create_thread scheduler () "light task scheduler")

  let () =
    ignore (Dtools.Init.at_start start)

end

(** Wait for some thread to crash *)
let main () = Mutex.lock no_problem
let shutdown () = Mutex.unlock no_problem
