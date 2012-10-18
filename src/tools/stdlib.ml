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
