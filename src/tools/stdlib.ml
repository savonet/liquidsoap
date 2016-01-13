let id x = x

module List = struct
  include List

  let init n f =
    let rec aux k =
      if k = n then [] else
        (f k)::(aux (k+1))
    in
    aux 0

  let rec may_map f = function
    | x::t ->
      (
        match f x with
        | Some x -> x::(may_map f t)
        | None -> may_map f t
      )
    | [] -> []

  let rec assoc_nth l n = function
    | [] -> raise Not_found
    | (x,v)::t when x = l ->
      if n = 0 then
        v
      else
        assoc_nth l (n-1) t
    | _::t -> assoc_nth l n t

  let assoc_all x l =
    may_map (fun (y,v) -> if x = y then Some v else None) l

  let rec last = function
    | [x] -> x
    | _::l -> last l
    | [] -> raise Not_found
end

module String = struct
  include String

  let split_char c s =
    let rec aux res n =
      try
        let n' = index_from s n c in
        let s0 = sub s n (n'-n) in
        aux (s0::res) (n'+1)
      with
      | Not_found ->
        (if n = 0 then s else sub s n (length s - n)) :: res
    in
    List.rev (aux [] 0)
end

module Unix = struct
  include Unix

  let read_retry s buf off len =
    let r = ref 0 in
    let loop = ref true in
    while !loop do
      let n = Unix.read s buf (off + !r) (len - !r) in
      r := !r + n;
      loop := !r <> 0 && !r < len
    done;
    !r
end
