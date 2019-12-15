(** Digesting strings according to SHA-1, see RFC 3174 and https://en.wikipedia.org/wiki/SHA-1 *)

module Int32 = struct
  include Int32

  let of_string_be s off =
    let n = ref Int32.zero in
    for k = 0 to 3 do
      n := shift_left !n 8;
      n := add !n (Int32.of_int (int_of_char s.[off + k]))
    done;
    !n

  let to_string_be n =
    let ans = Bytes.create 4 in
    let n = ref n in
    for i = 3 downto 0 do
      let c = Int32.to_int (logand !n 0xffl) in
      Bytes.set ans i (char_of_int c);
      n := Int32.shift_right !n 8
    done;
    Bytes.unsafe_to_string ans

  let bit n k =
    let mask = shift_left one k in
    logand n mask <> zero

  module String = struct
    let hexadecimal n =
      let ans = ref "" in
      for i = 3 downto 0 do
        let n = shift_right_logical n (i * 8) in
        let n = to_int n in
        let n = n land 0xff in
        ans := Printf.sprintf "%s%02x" !ans n
      done;
      !ans

    let binary n =
      let ans = ref "" in
      for i = 31 downto 0 do
        ans := !ans ^ if bit n i then "1" else "0"
      done;
      !ans
  end

  let msb =
    let mask = shift_left one 31 in
    fun n -> logand n mask <> zero

  let leftrotate n k = logor (shift_left n k) (shift_right_logical n (32 - k))
end

(** Digest a string using SHA-1. *)
let digest s =
  (* Pad string and append length. *)
  let len = String.length s in
  let s = s ^ String.make 1 (char_of_int 0b10000000) in
  let pad = 64 - ((len + 1) mod 64) - 8 in
  let pad = if pad < 0 then pad + 64 else pad in
  let pad = String.make pad (char_of_int 0) in
  let slen =
    let ans = Bytes.create 8 in
    let len = ref (8 * len) in
    for i = 7 downto 0 do
      Bytes.set ans i (char_of_int (!len land 0xff));
      len := !len lsr 8
    done;
    Bytes.unsafe_to_string ans
  in
  let s = s ^ pad ^ slen in
  let lnot = Int32.lognot in
  let ( land ) = Int32.logand in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( ++ ) = Int32.add in
  (* Main loop. *)
  let len = String.length s in
  assert (len mod 64 = 0);
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  for chunk = 0 to (len / 64) - 1 do
    (* Extend words. *)
    let w = Array.make 80 Int32.zero in
    for i = 0 to 15 do
      let off = (64 * chunk) + (4 * i) in
      w.(i) <- Int32.of_string_be s off
    done;
    for i = 16 to 79 do
      w.(i) <-
        Int32.leftrotate
          (w.(i - 3) lxor w.(i - 8) lxor w.(i - 14) lxor w.(i - 16))
          1
    done;
    (* Main loop. *)
    let a = ref !h0 in
    let b = ref !h1 in
    let c = ref !h2 in
    let d = ref !h3 in
    let e = ref !h4 in
    for i = 0 to 79 do
      let f =
        if i <= 19 then !b land !c lor (lnot !b land !d)
        else if i <= 39 then !b lxor !c lxor !d
        else if i <= 59 then !b land !c lor (!b land !d) lor (!c land !d)
        else !b lxor !c lxor !d
      in
      let k =
        if i <= 19 then 0x5A827999l
        else if i <= 39 then 0x6ED9EBA1l
        else if i <= 59 then 0x8F1BBCDCl
        else 0xCA62C1D6l
      in
      let temp = Int32.leftrotate !a 5 ++ f ++ !e ++ k ++ w.(i) in
      e := !d;
      d := !c;
      c := Int32.leftrotate !b 30;
      b := !a;
      a := temp
    done;
    h0 := Int32.add !h0 !a;
    h1 := Int32.add !h1 !b;
    h2 := Int32.add !h2 !c;
    h3 := Int32.add !h3 !d;
    h4 := Int32.add !h4 !e
  done;
  (* Printf.sprintf "%s %s %s %s %s" (Int32.String.hexadecimal !h0) (Int32.String.hexadecimal !h1) (Int32.String.hexadecimal !h2) (Int32.String.hexadecimal !h3) (Int32.String.hexadecimal !h4) *)
  Int32.to_string_be !h0 ^ Int32.to_string_be !h1 ^ Int32.to_string_be !h2
  ^ Int32.to_string_be !h3 ^ Int32.to_string_be !h4

(* let () = *)
(* (\* Example from Wikipedia page *\) *)
(* let s = "The quick brown fox jumps over the lazy dog" in *)
(* assert (digest s = "\x2f\xd4\xe1\xc6\x7a\x2d\x28\xfc\xed\x84\x9e\xe1\xbb\x76\xe7\x39\x1b\x93\xeb\x12") *)
