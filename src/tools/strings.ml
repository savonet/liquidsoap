(** Operations on lists of strings. This is module is used in order to avoid
    concatenating (large) strings. Iterators are FIFO. *)

module S = StringView

(* List of "concatenated" strings, stored backwards. *)
type t = S.t list

let empty = []

let of_string s : t = [S.of_string s]

let of_list l =
  let rec aux acc = function
    | [] -> acc
    | x::l -> aux ((S.of_string x)::acc) l
  in
  aux [] l

let dda x l = l@[S.of_string x]

let add_sv l x = x::l

let add (l:t) x : t = add_sv l (S.of_string x)

let add_subbytes l s o len = add l (Bytes.sub_string s o len)

let is_empty l = List.for_all S.is_empty l

let rec iter_sv f = function
  | [] -> ()
  | x::l -> iter_sv f l; f x

let iter f l =
  iter_sv (fun s -> f (S.to_string s)) l

let fold_sv f x0 l =
  let rec aux = function
    | [] -> x0
    | x::l -> f (aux l) x
  in
  aux l

let length l = fold_sv (fun n s -> n + S.length s) 0 l

let append l1 l2 = l2@l1

let concat ll = List.concat (List.rev ll)

let drop l len =
  let rec aux len = function
    | [] -> (len, [])
    | x::l ->
      let len, l = aux len l in
      if len = 0 then 0, x::l
      else
        let lx = S.length x in
        if len >= lx then (len-lx, l)
        else (0, (S.sub x len (lx-len))::l)
  in
  let r, l = aux len l in
  assert (r = 0);
  l

let sub l o len =
  assert (o + len <= length l);
  let o = ref o in
  let len = ref len in
  let ans = ref empty in
  iter_sv
    (fun s ->
       if !len = 0 then ()
       else
         let ls = S.length s in
         if !o >= ls then o := !o - ls
         else
           let r = min (ls - !o) !len in
           let s = S.sub s !o r in
           ans := add_sv !ans s;
           o := 0;
           len := !len - r
    ) l;
  assert (!len = 0);
  !ans

let blit l b o =
  let len = length l in
  assert (o + len <= Bytes.length b);
  let o = ref o in
  iter_sv
    (fun s ->
       S.blit s b !o;
       o := !o + S.length s
    ) l

let to_string l =
  let ans = Bytes.create (length l) in
  blit l ans 0;
  Bytes.unsafe_to_string ans

let substring l o len = to_string (sub l o len)
