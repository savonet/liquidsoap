(* Helpers the stdlib does not have.

   Flat on purpose. These used to live in `module List = struct include List
   ... end` and the same for Array, Unix and Int, so that a file doing `open
   Extralib` had the whole of List, Array and Unix silently redirected through
   here to pick up four extra functions. `module String` had ended up with
   nothing left in it at all. *)

let rec assoc_nth key n = function
  | [] -> raise Not_found
  | (k, v) :: t when k = key -> if n = 0 then v else assoc_nth key (n - 1) t
  | _ :: t -> assoc_nth key n t

let assoc_all key l =
  List.filter_map (fun (k, v) -> if k = key then Some v else None) l

let rec last = function [x] -> x | _ :: l -> last l | [] -> raise Not_found

(* The first [n] elements of [l], or all of them if it is shorter. *)
let rec prefix n l =
  match l with
    | [] -> []
    | x :: l -> if n = 0 then [] else x :: prefix (n - 1) l

(** Perfect Fisher-Yates shuffle
    (http://www.nist.gov/dads/HTML/fisherYatesShuffle.html). *)
let shuffle l =
  let a = Array.of_list l in
  let len = Array.length a in
  for i = 0 to len - 1 do
    let j = i + Random.int (len - i) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  Array.to_list a

(* Keep reading until [len] bytes have been read or [read] returns 0. *)
let read_retry read buf off len =
  let r = ref 0 in
  let loop = ref true in
  while !loop do
    let n = read buf (off + !r) (len - !r) in
    r := !r + n;
    loop := !r <> 0 && !r < len && n <> 0
  done;
  !r

let read_retry_fd fd = read_retry (Unix.read fd)
