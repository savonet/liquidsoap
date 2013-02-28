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

  let may_assoc x l =
    try
      Some (List.assoc x l)
    with
    | Not_found -> None

  let rec assoc_nth l n = function
    | [] -> raise Not_found
    | (x,v)::t when x = l ->
      if n = 0 then
        v
      else
        assoc_nth l (n-1) t
    | _::t -> assoc_nth l n t
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

module Option = struct
  let default d = function
    | Some x -> x
    | None -> d

  (** Functor part of the option monad. *)
  let funct f x =
    match x with
    | None -> None
    | Some x -> Some (f x)
end
