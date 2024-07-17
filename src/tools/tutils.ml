
open Dtools

let mutexify lock f =
  fun x ->
    Mutex.lock lock ;
    try
      let ans = f x in Mutex.unlock lock ; ans
    with
      | e -> Mutex.unlock lock ; raise e

module Mutex =
struct

  let lock = Mutex.create ()
  let h = Hashtbl.create 99
  let create =
    mutexify lock
      (fun s ->
         let m = Mutex.create () in
           Hashtbl.add h s m ;
           m)

  let is_locked m = Mutex.try_lock m && ( Mutex.unlock m ; true )
  let get_list =
    mutexify lock
      (fun () ->
         Hashtbl.fold (fun k v l ->
                         (k,(is_locked v),v)::l) h [])

  (* Just for this file *)
  let lock = Mutex.lock
  let unlock = Mutex.unlock

end

let lock = Mutex.create "Tutils.lock"
let uncaught = ref None
module Set = Set.Make (struct
                         type t = (string*Thread.t)
                         let compare = compare
                       end)
let all = ref (Set.empty)

let running s id =
  Set.mem (s,id) !all

let join_all_no_timeout () =
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

let join_all () =
  let pid = Unix.fork () in
    if pid = 0 then join_all_no_timeout () else
      begin
        Unix.sleep 3 ;
        (try Unix.kill pid Sys.sigkill with _ -> ()) ;
        ignore (Unix.wait ())
      end

let no_problem =
  let m = Mutex.create "Tutils.no_problem" in
    Mutex.lock m ;
    m

let logl = Log.logl ~label:"threads"

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
               logl 3 (lazy (Log.f "%S exited (%d remaining)"
                               s (Set.cardinal !all))) ;
               Mutex.unlock lock
	     with e ->
	       Mutex.lock lock ; 
	       logl 1 (lazy (Log.f "%S aborts with exception %s !" 
			       s (Printexc.to_string e))) ;
	       all := Set.remove (s,(Thread.self ())) !all ;
	       uncaught := Some e ;
	       Mutex.unlock no_problem ;
	       Mutex.unlock lock)
	  x
      in
        all := Set.add (s,id) !all ;
        logl 3 (lazy (Log.f "created %S (%d total)" s (Set.cardinal !all))) ;
	id
  ) ()

let raise_everything () =
  Mutex.lock no_problem ;
  match !uncaught with
    | None -> assert false
    | Some e -> ()

let get_list () =
  Set.fold (fun e l -> e::l) !all []

