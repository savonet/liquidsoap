
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*        François Pottier, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(*         Modifications by David Baelde, ENS Lyon, May 2003           *)

exception Empty

type 'a cell = {
    content : 'a ;
    mutable next : 'a cell ;
    mutable prev : 'a cell ;
  } 

(* 
 *
 *  Realistic queues FIFO & LIFO, thread-safe :
 *  
 *       TOP [ rqueue ] BOTTOM
 *     push> [ rqueue ] >pop
 *    shift< [ rqueue ] <unshift
 *
 *  [ ... cell -(next)-> cell ... ]
 *
 *)

type 'a t = {
  lock : Mutex.t ;
  mutable length : int ;
  mutable top : 'a cell
} 

let create () = 
  {
    lock = Mutex.create () ;
    length = 0 ;
    top = Obj.magic None
  } 

let clear q =
  Mutex.lock q.lock ;
  q.length <- 0 ;
  q.top <- Obj.magic None ;
  Mutex.unlock q.lock

let top q =
  Mutex.lock q.lock ;
  match q.length with
    | 0 ->
	Mutex.unlock q.lock ; 
	raise Empty 
    | _ ->
	let ans = q.top.content in
	  Mutex.unlock q.lock ;
	  ans

let length q =
  Mutex.lock q.lock ;
  let ans = q.length in
    Mutex.unlock q.lock ;
    ans

let is_empty q = 
  0 = length q

let to_list q =
  Mutex.lock q.lock ;
  if q.length = 0 then (Mutex.unlock q.lock ; []) else
    begin

      let stop = q.top in
      let rec aux ans = fun c ->
	if c.next == stop
	then List.rev (c.content::ans)
	else aux (c.content::ans) c.next 

      in
      let ans = aux [] q.top in
	Mutex.unlock q.lock ;
	ans
    end

let push q x =
  Mutex.lock q.lock ;
  q.length <- q.length + 1 ;
  match q.length with
    | 1 ->
	let rec cell = {
	  content = x ;
	  next = cell ;
	  prev = cell
	} in
	  q.top <- cell ;
	  Mutex.unlock q.lock ;
    | _ ->
	let head = q.top in
	let tail = head.prev in
	let newtop =
	  {
	    content = x ;
	    next = head ;
	    prev = tail ;
	  } in
	  q.top <- newtop ;
	  head.prev <- newtop ;
	  tail.next <- newtop ;
	  Mutex.unlock q.lock 

let unshift q x =
  Mutex.lock q.lock ;
  q.length <- q.length + 1 ;
  match q.length with
    | 1 ->
	let rec cell = {
	  content = x ;
	  next = cell ;
	  prev = cell
	} in
	  q.top <- cell ;
	  Mutex.unlock q.lock ;
    | _ ->
	let head = q.top in
	let tail = head.prev in
	let newtail =
	  {
	    content = x ;
	    next = head ;
	    prev = tail ;
	  } in
	  head.prev <- newtail ;
	  tail.next <- newtail ;
	  Mutex.unlock q.lock 
	
let pop q =
  Mutex.lock q.lock ;
  match q.length with
    | 0 ->
	Mutex.unlock q.lock ;
	raise Empty
    | 1 ->
	q.length <- 0 ;
	let ans = q.top.content in
	  q.top <- Obj.magic None ;
	  Mutex.unlock q.lock ;
	  ans
    | n ->
	q.length <- n-1 ;
	let tail = q.top.prev in
	let head = q.top in
	let ans = tail.content in
	let newtail = tail.prev in
	  head.prev <- newtail ;
	  newtail.next <- head ;
	  Mutex.unlock q.lock ;
	  ans

let shift q =
  Mutex.lock q.lock ;
  match q.length with
    | 0 ->
	Mutex.unlock q.lock ; 
	raise Empty 
    | 1 ->
	q.length <- 0 ;
	let ans = q.top.content in
	  q.top <- Obj.magic None ;
	  Mutex.unlock q.lock ;
	  ans
    | n ->
	q.length <- n-1 ;
	let head = q.top in
	let tail = head.prev in
	let newhead = head.next in
	  q.top <- newhead ;
	  newhead.prev <- tail ;
	  tail.next <- newhead ;
	  let ans = head.content in
	    Mutex.unlock q.lock ;
	    ans

