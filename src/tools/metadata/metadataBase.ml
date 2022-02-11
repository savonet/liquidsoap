(** Raised when the format is invalid. *)
exception Invalid

type metadata = (string * string) list

type endianness = Big_endian | Little_endian

module Reader = struct
  (** A function to read taking the buffer to fill the offset and the length and
      returning the number of bytes actually read. *)
  type t = bytes -> int -> int -> int

  let retry (read : t) : t = fun buf off len ->
    let r = ref 0 in
    let loop = ref true in
    while !loop do
      let n = read buf (off + !r) (len - !r) in
      r := !r + n;
      loop := !r <> 0 && !r < len && n <> 0
    done;
    !r

  let read f n =
    let s = Bytes.create n in
    let k = retry f s 0 n in
    if k <> n then raise Invalid;
    Bytes.unsafe_to_string s

  let byte f = int_of_char (read f 1).[0]

  let int16_be f =
    let b0 = byte f in
    let b1 = byte f in
    b0 lsl 8 + b1

  let int16_le f =
    let b0 = byte f in
    let b1 = byte f in
    b1 lsl 8 + b0

  let int16 = function
    | Big_endian -> int16_be
    | Little_endian -> int16_le

  let int32_be f =
    let b0 = byte f in
    let b1 = byte f in
    let b2 = byte f in
    let b3 = byte f in
    b0 lsl 24 + b1 lsl 16 + b2 lsl 8 + b3

  let with_file f fname =
    let fd = Unix.openfile fname [] 0o644 in
    try
      let ans = f (Unix.read fd) in
      Unix.close fd;
      ans
    with e ->
      Unix.close fd;
      raise e
end

module CharEncoding = struct
  module C = CamomileLibraryDefault.Camomile.CharEncoding

  let iso8859 = C.of_name "ISO-8859-1"

  let utf8 = C.utf8

  let utf16 = C.utf16

  let convert in_enc out_enc s =
    C.recode_string ~in_enc ~out_enc s
end

module Int = struct
  include Int

  let find p =
    let ans = ref 0 in
    try
      while true do
        if p !ans then raise Exit else incr ans
      done;
      assert false
    with Exit -> !ans
end
