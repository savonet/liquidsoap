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
end

module String = struct
  include String

  let split_char c s =
    let rec aux n =
      try
        let n' = index_from s n c in
        let s0 = sub s n (n'-n) in
        s0 :: (aux (n'+1))
      with
      | Not_found ->
        if n = 0 then [s] else [sub s n (length s - n)]
    in
    aux 0
end
