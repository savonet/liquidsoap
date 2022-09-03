(** Raised when the format is invalid. *)
exception Invalid

type metadata = (string * string) list

type endianness = Big_endian | Little_endian

module Reader = struct
  (** A function to read taking the buffer to fill the offset and the length and
      returning the number of bytes actually read. *)
  type t =
    {
      read : bytes -> int -> int -> int;
      seek : int -> unit;
      size : unit -> int option;
      reset : unit -> unit
    }

  (** Make a reading function retry until buffer is filled (or an error
      occurs). *)
  let retry read = fun buf off len ->
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
    let k = retry f.read s 0 n in
    if k <> n then raise Invalid;
    Bytes.unsafe_to_string s

  let drop f n = f.seek n

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

  let int32_le f =
    let b0 = byte f in
    let b1 = byte f in
    let b2 = byte f in
    let b3 = byte f in
    b3 lsl 24 + b2 lsl 16 + b1 lsl 8 + b0

  let int32_be f =
    let b0 = byte f in
    let b1 = byte f in
    let b2 = byte f in
    let b3 = byte f in
    b0 lsl 24 + b1 lsl 16 + b2 lsl 8 + b3

  let size f = f.size ()

  (** Go back at the beginning of the stream. *)
  let reset f = f.reset ()

  let with_file f fname =
    let fd = Unix.openfile fname [] 0o644 in
    let file =
      let read = Unix.read fd in
      let seek n = ignore (Unix.lseek fd n Unix.SEEK_CUR) in
      let size () =
        try
          let p = Unix.lseek fd 0 Unix.SEEK_CUR in
          let n = Unix.lseek fd 0 Unix.SEEK_END in
          ignore (Unix.lseek fd p Unix.SEEK_SET);
          Some n
        with _ -> None
      in
      let reset () = ignore (Unix.lseek fd 0 Unix.SEEK_SET) in
      { read; seek; size; reset }
    in
    try
      let ans = f file in
      Unix.close fd;
      ans
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Unix.close fd;
      Printexc.raise_with_backtrace e bt
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
