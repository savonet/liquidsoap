
type expr = Scheduling_defs.expr

open Scheduling_defs

let never () = Never
let always () = WeekDay (0,6)

let from_string s = 
  try
    let lexbuf = Lexing.from_string s in
      Scheduling_parser.schedule Scheduling_lexer.token lexbuf
  with
    | Failure s -> 
	Printf.fprintf stderr "[WW] Failure : %s\n" s ;
	flush stderr ;
	Never
    | Parsing.Parse_error ->
	Printf.fprintf stderr "[WW] Parse error !\n" ;
	flush stderr ;
	Never

let rec to_string  = function
  | WeekDay (a,b) -> (string_of_int a)^"-"^(string_of_int b)^"w"
  | Hour (a,b) -> (string_of_int a)^"-"^(string_of_int b)^"h"
  | Minute (a,b) -> (string_of_int a)^"-"^(string_of_int b)^"m"
  | Second (a,b) -> (string_of_int a)^"-"^(string_of_int b)^"s"
  | And (f,g) -> "("^(to_string f)^" & "^(to_string g)^")"
  | Or (f,g) -> "("^(to_string f)^" | "^(to_string g)^")"
  | Never -> "Never"

let satisfied_with_offset f offset =
  let tm = Unix.localtime ((Unix.time ()) +. (float offset)) in
  let rec aux k f =

    let check a b now =
      if a <= b then
	k (a <= now && now <= b)	(* ... a now b ... *)
      else
	k (now >= a || b >= now)  	(* now a ... b now *)
    in

      match f with 
	| And (a,b) -> aux (fun i -> if i then aux k b else k false) a 
	| Or (a,b) -> aux (fun i -> if i then k true else aux k b) a 

	| WeekDay (a,b) -> check a b tm.Unix.tm_wday
	| Hour (a,b)    -> check a b tm.Unix.tm_hour
	| Minute (a,b)  -> check a b tm.Unix.tm_min
	| Second (a,b)  -> check a b tm.Unix.tm_sec

	| Never -> k false

  in aux (fun b -> b) f

let satisfied f =
  satisfied_with_offset f 0

(*
 *
 * Two utilities for the type system ...
 * When typing, one must be able to compute the disjunction of two expressions,
 * and check that two expressions do not intersect.
 *
 * In order to do that, we reduce every expr to a normal form,
 * of the type cexpr, and work then on it.
 *
 *)

type cexpr = (int*int) list

(** Normalization : handle with care ... *)

let order i1 i2 =
  match (fst i1) - (fst i2) with
    | 0 -> 0
    | n when n>=0 -> 1
    | _ -> -1

let sort = List.sort order

let merge = 
  let rec aux a b accu = function
    | [] -> List.rev ((a,b)::accu)
    | (aa,bb)::t -> 
	assert (aa<=bb) ;
	(** We assume aa>=a (sort), and aa<=bb (def). *)
	if aa<=b+1 then aux a (max b bb) accu t else aux aa bb ((a,b)::accu) t 
  in
    function
      | [] -> []
      | (a,b)::t -> assert (a<=b) ; aux a b [] t 

let normalize i = merge (sort i)

(** Disjunction of compiled expressions. Preserve the normalization. *)
let disjunction a b=
  let rec f a b acc =
    match (a,b) with
      | (ax,ay)::ta, (bx,by)::tb when ax<bx ->
	  f ta tb ((bx,by)::(ax,ay)::acc)
      | (ax,ay)::ta, (bx,by)::tb ->
	  f ta tb ((ax,ay)::(bx,by)::acc)
      | [], b -> (List.rev acc)@b
      | a, [] -> (List.rev acc)@a
  in merge (f a b [])

let rec elem_intersection (a,b) (aa,bb) =
  if a<=aa then 
    if b<aa 
    then []
    else [aa,(min b bb)]
  else 
    elem_intersection (aa,bb) (a,b)

(** Conjunction of compiled expressions. Preserve the normalization.
  Actually, [a] doesn't even need to be normalized. *)
let conjunction a b =
  (* (a | b | c) & (d | e | f) = ((a&d) | (a&e) | (a&f))
                               | ((b&d) | (b&e) | (b&f)) ... *) 
  let rec f a acc =
    if a = [] then acc else
      f (List.tl a) 
	(disjunction acc
	   (List.concat
	      (List.map (fun b -> elem_intersection (List.hd a) b) b)))
  in
    f a []

(** Compilation *)

let slen = 1
let mlen = 60*slen
let hlen = 60*mlen
let wlen = 24*hlen

let compiled_never () = []
let compiled_always () = [0,7*wlen-1]

let compile f =

  let rec replicate interval steps offset =
    if steps = 0 then [] else
      interval::
	(replicate 
	   ((fst interval)+offset,(snd interval)+offset)
	   (steps-1) 
	   offset)
  in
    
  let rec aux k = function

    | WeekDay (a,b) ->
	let il =
	  if a<=b 
	  then [a*wlen,(b+1)*wlen-1] 
	  else [(0,(b+1)*wlen-1);(a*wlen,7*wlen-1)] ;
	in
	 k (List.concat (List.map (fun i -> replicate i (1) (7*24*60*60)) il))
	    
    | Hour (a,b) ->
    	let il =
	  if a<=b 
	  then [a*hlen,(b+1)*hlen-1]
	  else [0,(b+1)*hlen-1;a*hlen,24*hlen-1]
	in
	  k (List.concat (List.map (fun i -> replicate i (1*7) (24*60*60)) il))
			
    | Minute (a,b) ->
    	let il =
	  if a<=b 
	  then [a*mlen,(b+1)*mlen-1]
	  else [0,(b+1)*mlen-1;a*mlen,60*mlen-1]
	in
	  k (List.concat (List.map (fun i -> replicate i (7*24) (60*60)) il))
			
    | Second (a,b) ->
    	let il =
	  if a<=b 
	  then [a*slen,(b+1)*slen-1]
	  else [0,(b+1)*slen-1;a*slen,60*slen-1]
	in
	  k (List.concat (List.map (fun i -> replicate i (7*24*60) (60)) il))
		
    | And (f,g) -> aux (fun cf -> aux (fun cg -> k (conjunction cf cg)) g) f
    | Or  (f,g) -> aux (fun cf -> aux (fun cg -> k (disjunction cf cg)) g) f

    | Never -> k []

  in normalize (aux (fun i -> i) f)

let intersect f g =
  conjunction f g <> []

let rec cexpr_to_string = function
  | [] -> ""
  | (a,b)::r -> 
      ((string_of_int a)^"-"^(string_of_int b))
      ^","^
      (cexpr_to_string r)
