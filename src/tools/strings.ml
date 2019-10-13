(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** Operations on lists of strings. This is module is used in order to avoid
    concatenating (large) strings. Iterators are FIFO. *)

type entry = {
  mutable start: int;
  mutable stop: int;
  data: string
}

type mode = [
  | `RW
  | `RO
]

type 'a t = {
  entries: entry Queue.t;
  mutex: Mutex.t
}

(* This is duplicated from Tutils to avoid circular dependencies. *)
let mutexify lock f =
  fun x ->
    Mutex.lock lock ;
    try
      let ans = f x in Mutex.unlock lock ; ans
    with
      | e -> Mutex.unlock lock ; raise e

let entry_of_string s = {
  start = 0;
  stop  = String.length s;
  data  = s
}

let empty () = {
  entries = Queue.create ();
  mutex = Mutex.create ()
}

let of_string string =
  let t = empty () in
  Queue.push (entry_of_string string) t.entries;
  t

let of_bytes b =
  of_string (Bytes.to_string b)

let unsafe_of_bytes b =
  of_string (Bytes.unsafe_to_string b)

let of_list l =
  let t = empty () in
  let l = List.filter (fun string ->
    String.length string > 0) l
  in
  let l = List.map entry_of_string l in
  List.iter (fun entry -> Queue.push entry t.entries) l;
  t

let of_bytes_list l =
  of_list (List.map Bytes.to_string l)

let unsafe_of_bytes_list l =
  of_list (List.map Bytes.unsafe_to_string l)

let copy_queue t =
  let q = Queue.create () in
  Queue.iter (fun {start;stop;data} ->
    Queue.add {start;stop;data} q) t.entries;
  q

let copy_unsafe t = {
  (empty ()) with
     entries = copy_queue t
}

let copy t = mutexify t.mutex (fun () ->
  copy_unsafe t) ()

let seal = copy

let prepend string t = mutexify t.mutex (fun () ->
  let entries = Queue.create () in
  let t_q = Queue.copy t.entries in
  Queue.add (entry_of_string string) entries;
  Queue.transfer t_q entries;
  {(empty()) with entries}) ()

let prepend_bytes b =
  prepend (Bytes.to_string b)

let unsafe_prepend_bytes b =
  prepend (Bytes.unsafe_to_string b)

let add t = mutexify t.mutex (fun string ->
  if String.length string > 0 then
    Queue.push (entry_of_string string) t.entries)

let add_bytes t b =
  add t (Bytes.to_string b)

let unsafe_add_bytes t b =
  add t (Bytes.unsafe_to_string b)

let add_substring t data ofs len =
  mutexify t.mutex (fun () ->
    if String.length data < ofs+len then
      raise (Invalid_argument "Bytes_buffer.add_substring");
    if len > 0 then
      Queue.push {start = ofs; stop = ofs+len; data} t.entries) ()

let add_subbytes t data ofs len =
  if Bytes.length data < ofs+len then
    raise (Invalid_argument "Bytes_buffer.add_substring");
  add_substring t (Bytes.to_string data) ofs len

let unsafe_add_subbytes t data ofs len =
  if Bytes.length data < ofs+len then
    raise (Invalid_argument "Bytes_buffer.add_substring");
  add_substring t (Bytes.unsafe_to_string data) ofs len

let length_unsafe t =
  Queue.fold (fun count {start;stop} ->
    count + stop - start) 0 t.entries

let length t = mutexify t.mutex (fun () ->
  length_unsafe t) ()

let is_empty t = length t = 0

let iter fn t = mutexify t.mutex (fun () ->
  Queue.iter (fun {start;stop;data} ->
    fn data start (start-stop)) t.entries) ()

let fold fn init t = mutexify t.mutex (fun () ->
  Queue.fold (fun cur {start;stop;data} ->
    fn cur data start (stop-start)) init t.entries) ()

let append t1 t2 = mutexify t1.mutex (fun () ->
   mutexify t2.mutex (fun () ->
      Queue.iter (fun {start;stop;data} ->
         Queue.push {start;stop;data} t1.entries) t2.entries) ()) ()

let concat l =
  let t = empty () in
  List.iter (append t) l;
  t

let drop_unsafe t len =
  let rec f pending =
    let {start;stop} as entry = Queue.peek t.entries in
    let len = stop-start in
    if pending < len then
      entry.start <- entry.start+pending
    else
     begin
      ignore(Queue.take t.entries);
      f (pending-len)
     end
  in
  try f len with Queue.Empty -> ()

let drop t = mutexify t.mutex (drop_unsafe t)

let keep_unsafe t len =
 let current_length = length_unsafe t in
 if current_length > len then
   drop_unsafe t (current_length-len)

let keep t = mutexify t.mutex (keep_unsafe t)

let sub_unsafe t ofs len =
  let length = length_unsafe t in
  if length-ofs < len then
    raise (Invalid_argument "Bytes_buffer.sub");
  let t = copy_unsafe t in
  drop_unsafe t ofs;
  let q = Queue.create () in
  let rec f pending =
    let {start;stop} as entry = Queue.take t.entries in
    Queue.add entry q;
    let entry_len = stop-start in
    if pending <= entry_len then
      entry.stop <- entry.stop-entry_len+pending
    else
      f (pending-entry_len)
  in
  begin
   try
    if len > 0 then f len
   with Queue.Empty -> assert false
  end;
  {t with entries = q}

let sub t ofs len = mutexify t.mutex (fun () ->
  sub_unsafe t ofs len) ()

let blit_unsafe t ofs_t target ofs len =
  let t = sub_unsafe t ofs_t len in
  ignore(Queue.fold (fun pos {start;stop;data} ->
    let len = stop-start in
    Bytes.blit_string data start target pos len;
    pos+len) ofs t.entries)

let blit t ofs_t target ofs len = mutexify t.mutex (fun () ->
  blit_unsafe t ofs_t target ofs len) ()  

let to_bytes t = mutexify t.mutex (fun () ->
  let len = length_unsafe t in
  let bytes = Bytes.create len in
  blit_unsafe t 0 bytes 0 len;
  bytes) ()

let to_string t =
  Bytes.unsafe_to_string (to_bytes t)

let flush t = mutexify t.mutex (fun () ->
  Queue.clear t.entries) ()

let to_list t = mutexify t.mutex (fun () ->
  List.of_seq
    (Seq.map (fun ({start;stop;data}) ->
      data,start,stop-start)
        (Queue.to_seq t.entries))) ()

let to_bytes_list t =
  List.map (fun (data,ofs,len) ->
    Bytes.of_string data,ofs,len) (to_list t)

let unsafe_to_bytes_list t =
  List.map (fun (data,ofs,len) ->
    Bytes.unsafe_of_string data,ofs,len) (to_list t)
