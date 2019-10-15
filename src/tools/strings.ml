(** Operations on lists of strings. This is module is used in order to avoid
    concatenating (large) strings. Iterators are FIFO. *)

(* List of "concatenated" strings, stored backwards. *)
type t = string list

let empty = []

let of_string s : t = [s]

let of_list l = List.rev l

let dda x l = l@[x]

let add (l:t) x : t = x::l

let add_subbytes t buf ofs len =
  add t (Bytes.sub_string buf ofs len)

let add_subbytes l s o len = add l (Bytes.sub_string s o len)

let is_empty l = List.for_all (fun s -> s = "") l

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

let concat ll = List.concat (List.rev ll)

let drop l len =
  let rec aux len = function
    | [] -> (len, [])
    | x::l ->
      let len, l = aux len l in
      if len = 0 then 0, x::l
      else
        let lx = String.length x in
        if len >= lx then (len-lx, l)
        else (0, (String.sub x len (lx-len))::l)
  in
  let r, l = aux len l in
  assert (r = 0);
  l

let keep l len =
  let rec aux len = function
    | _ when len <= 0 -> []
    | x::l -> x::(aux (len - String.length x) l)
    | [] -> []
  in
  aux len l

let sub l o len =
  assert (o + len <= length l);
  let o = ref o in
  let len = ref len in
  let ans = ref empty in
  iter
    (fun s ->
       if !len = 0 then ()
       else
         let ls = String.length s in
         if !o >= ls then o := !o - ls
         else
           let r = min (ls - !o) !len in
           let s =
             if !o = 0 && r = ls then s
             else String.sub s !o r
           in
           ans := add !ans s;
           o := 0;
           len := !len - r
    ) l;
  assert (!len = 0);
  !ans

(* We cannot share the code with sub because we don't want to uselessly sub the last string... *)
let blit l o b ob len =
  assert (o + len <= length l);
  assert (ob + len <= Bytes.length b);
  let o = ref o in
  let ob = ref ob in
  let len = ref len in
  iter
    (fun s ->
       if !len = 0 then ()
       else
         let ls = String.length s in
         if !o >= ls then o := !o - ls
         else
           let r = min (ls - !o) !len in
           String.blit s !o b !ob r;
           o := 0;
           ob := !ob + r;
           len := !len - r
    ) l;
  assert (!len = 0)

let to_string_list l = List.rev l

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

let substring l o len = to_string (sub l o len)
