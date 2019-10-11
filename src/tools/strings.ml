(** Operations on lists of strings. This is module is used in order to avoid
    concatenating (large) strings. Iterators are FIFO. *)

(* List of "concatenated" strings, stored backwards. *)
type t = string list

let empty = []

let of_string s : t = [s]

let of_list l = List.rev l

let dda x l = l@[x]

let add (l:t) x : t = x::l

let add_subbytes l s o len = add l (Bytes.sub_string s o len)

let rec iter f = function
  | [] -> ()
  | x::l -> iter f l; f x

let fold f x0 l =
  let rec aux = function
    | [] -> x0
    | x::l -> f (aux l) x
  in
  aux l

let length l = fold (fun n s -> n + String.length s) 0 l

let append l1 l2 = l2@l1

let concat ll = List.concat ll

let to_string l =
  let ans = Bytes.create (length l) in
  let o = ref 0 in
  iter
    (fun s ->
       let len = String.length s in
       Bytes.blit_string s 0 ans !o len;
       o := !o + len
    ) l;
  Bytes.unsafe_to_string ans
