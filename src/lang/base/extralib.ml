let id x = x

module Array = struct
  include Array

  (** Perfect Fisher-Yates shuffle
      (http://www.nist.gov/dads/HTML/fisherYatesShuffle.html). *)
  let shuffle a =
    let permute i j =
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
    in
    let l = Array.length a in
    for i = 0 to l - 1 do
      permute i (i + Random.int (l - i))
    done
end

module List = struct
  include List

  let rec assoc_nth l n = function
    | [] -> raise Not_found
    | (x, v) :: t when x = l -> if n = 0 then v else assoc_nth l (n - 1) t
    | _ :: t -> assoc_nth l n t

  let assoc_all x l =
    filter_map (fun (y, v) -> if x = y then Some v else None) l

  let rec last = function [x] -> x | _ :: l -> last l | [] -> raise Not_found

  let rec prefix n l =
    match l with
      | [] -> []
      | x :: l -> if n = 0 then [] else x :: prefix (n - 1) l

  let shuffle l =
    let a = Array.of_list l in
    Array.shuffle a;
    Array.to_list a
end

module String = struct
  include String

  let ends_with s a =
    let ls = String.length s in
    let la = String.length a in
    ls >= la && String.sub s (ls - la) la = a

  let split_char c s =
    let rec aux res n =
      try
        let n' = index_from s n c in
        let s0 = sub s n (n' - n) in
        aux (s0 :: res) (n' + 1)
      with Not_found -> (if n = 0 then s else sub s n (length s - n)) :: res
    in
    List.rev (aux [] 0)
end

let read_retry read buf off len =
  let r = ref 0 in
  let loop = ref true in
  while !loop do
    let n = read buf (off + !r) (len - !r) in
    r := !r + n;
    loop := !r <> 0 && !r < len && n <> 0
  done;
  !r

module Unix = struct
  include Unix

  let read_retry fd = read_retry (read fd)
end

module Int = struct
  include Int

  let find p =
    let ans = ref 0 in
    try
      while true do
        if p !ans then raise Exit else incr ans
      done;
      assert false
    with Exit -> !ans
end

module Fun = struct
  include Fun

  (** Execute a function at most once. *)
  let once =
    let already = ref false in
    fun f ->
      if not !already then (
        already := true;
        f ())
end
