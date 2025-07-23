include Stdlib.List

let[@tail_mod_cons] rec map f = function
  | [] -> []
  | [a1] ->
      let r1 = f a1 in
      [r1]
  | a1 :: a2 :: l ->
      let r1 = f a1 in
      let r2 = f a2 in
      r1 :: r2 :: map f l

let[@tail_mod_cons] rec mapi i f = function
  | [] -> []
  | [a1] ->
      let r1 = f i a1 in
      [r1]
  | a1 :: a2 :: l ->
      let r1 = f i a1 in
      let r2 = f (i + 1) a2 in
      r1 :: r2 :: mapi (i + 2) f l

let mapi f l = mapi 0 f l
