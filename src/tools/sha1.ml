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
    let ans = ref "" in
    for i = 3 downto 0 do
      let n = Int32.shift_right_logical n (i * 8) in
      let n = Int32.to_int n in
      let n = n land 0xff in
      ans := Printf.sprintf "%s%x" !ans n
    done;
    !ans

  let msb =
    let mask = Int32.shift_left Int32.one 63 in
    fun n -> logand n mask <> Int32.zero

  let leftrotate n =
    let m = msb n in
    let n = shift_left n 1 in
    if m then Int32.logor n Int32.one else n

  let leftrotate n k =
    let n = ref n in
    for i = 0 to k - 1 do
      n := leftrotate !n
    done;
    !n

  module List = struct
    let add l = List.fold_left add zero l
  end
end

let h0 = Int32.of_string "0x67452301"
let h1 = Int32.of_string "0xEFCDAB89"
let h2 = Int32.of_string "0x98BADCFE"
let h3 = Int32.of_string "0x10325476"
let h4 = Int32.of_string "0xC3D2E1F0"

let k0 = Int32.of_string "0x5A827999"
let k1 = Int32.of_string "0x6ED9EBA1"
let k2 = Int32.of_string "0x8F1BBCDC"
let k3 = Int32.of_string "0xCA62C1D6"

let encode s =
  (* Pad string and append length. *)
  let len = String.length s in
  let s = s ^ (String.make 1 (char_of_int 0b10000000)) in
  let pad = 64 - ((len+1) mod 64) - 8 in
  let pad = if pad < 0 then pad + 64 else pad in
  let pad = String.make pad (char_of_int 0) in
  let slen =
    let ans = String.create 8 in
    let len = ref len in
    for i = 7 downto 0 do
      ans.[i] <- char_of_int (!len land 0xff);
      len := !len lsr 8;
    done;
    ans
  in
  let s = s ^ pad ^ slen in

  (* Main loop. *)
  let len = String.length s in
  assert (len mod 64 = 0);
  let h0 = ref h0 in
  let h1 = ref h1 in
  let h2 = ref h2 in
  let h3 = ref h3 in
  let h4 = ref h4 in

  for chunk = 0 to len / 64 - 1 do
    (* Extend words. *)
    let w = Array.make 80 Int32.zero in
    for i = 0 to 15 do
      let off = 64 * chunk + 4 * i in
      w.(i) <- Int32.of_string_be s off
    done;
    for i = 16 to 79 do
      w.(i) <- Int32.leftrotate (Int32.logxor w.(i-3) (Int32.logxor w.(i-8) (Int32.logxor w.(i-14) w.(i-16)))) 1
    done;

    (* Main loop. *)
    let a = ref !h0 in
    let b = ref !h1 in
    let c = ref !h2 in
    let d = ref !h3 in
    let e = ref !h4 in

    for i = 0 to 79 do
      let f, k =
        if i <= 19 then (Int32.logor (Int32.logand !b !c) (Int32.logand (Int32.lognot !b) !d), k0)
        else if i <= 39 then (Int32.logxor !b (Int32.logxor !c !d), k1)
        else if i <= 59 then (Int32.logor (Int32.logand !b !c) (Int32.logor (Int32.logand !b !d) (Int32.logand !c !d)), k2)
        else (Int32.logxor !b (Int32.logxor !c !d), k3)
      in
      let temp = Int32.List.add [Int32.leftrotate !a 5; f; !e; k; w.(i)] in
      e := !d;
      d := !c;
      c := Int32.leftrotate !b 30;
      b := !a;
      a := temp;
    done;

    h0 := Int32.add !h0 !a;
    h1 := Int32.add !h1 !b;
    h2 := Int32.add !h2 !c;
    h3 := Int32.add !h3 !d;
    h4 := Int32.add !h4 !e
  done;
  Printf.sprintf "%s %s %s %s %s" (Int32.to_string_be !h0) (Int32.to_string_be !h1) (Int32.to_string_be !h2) (Int32.to_string_be !h3) (Int32.to_string_be !h4)

let () =
  let s = "The quick brown fox jumps over the lazy dog" in
  Printf.printf "SHA1:\n%S\n%s\n%!" s (encode s)
