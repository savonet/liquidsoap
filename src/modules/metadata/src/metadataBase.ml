(** Raised when the format is invalid. *)
exception Invalid

type bigarray =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type metadata = (string * string) list
type endianness = Big_endian | Little_endian

type parser_handler = {
  label : string;
  length : int;
  read : unit -> string;
  read_ba : (unit -> bigarray) option;
  skip : unit -> unit;
}

type custom_parser = parser_handler -> unit

module Reader = struct
  (** A function to read taking the buffer to fill the offset and the length and
      returning the number of bytes actually read. *)
  type t = {
    read : bytes -> int -> int -> int;
    read_ba : (int -> bigarray) option;
    custom_parser : custom_parser option;
    seek : int -> unit;
    size : unit -> int option;
    reset : unit -> unit;
  }

  (** Make a reading function retry until buffer is filled (or an error occurs).
  *)
  let retry read buf off len =
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

  let read_tag ~length ~label f =
    let is_custom =
      match f.custom_parser with
        | None -> false
        | Some custom_parser ->
            let is_custom = ref false in
            let skip () =
              is_custom := true;
              f.seek length
            in
            let read () =
              is_custom := true;
              read f length
            in
            let read_ba =
              Option.map
                (fun read_ba () ->
                  is_custom := true;
                  read_ba length)
                f.read_ba
            in
            custom_parser { read_ba; read; skip; length; label };
            !is_custom
    in
    if is_custom then None else Some (read f length)

  let drop f n = f.seek n
  let byte f = int_of_char (read f 1).[0]
  let uint8 f = byte f

  let int16_be f =
    let b0 = byte f in
    let b1 = byte f in
    (b0 lsl 8) + b1

  let int16_le f =
    let b0 = byte f in
    let b1 = byte f in
    (b1 lsl 8) + b0

  let uint16_le = int16_le
  let int16 = function Big_endian -> int16_be | Little_endian -> int16_le

  let int24_be f =
    let b0 = byte f in
    let b1 = byte f in
    let b2 = byte f in
    (b0 lsl 16) + (b1 lsl 8) + b2

  let int32_le f =
    let b0 = byte f in
    let b1 = byte f in
    let b2 = byte f in
    let b3 = byte f in
    (b3 lsl 24) + (b2 lsl 16) + (b1 lsl 8) + b0

  let uint32_le = int32_le

  let int32_be f =
    let b0 = byte f in
    let b1 = byte f in
    let b2 = byte f in
    let b3 = byte f in
    (b0 lsl 24) + (b1 lsl 16) + (b2 lsl 8) + b3

  let size f = f.size ()
  let reset f = f.reset ()

  let with_file ?custom_parser f fname =
    let fd = Unix.openfile fname [Unix.O_RDONLY; Unix.O_CLOEXEC] 0o644 in
    let file =
      let read = Unix.read fd in
      let read_ba len =
        let pos = Int64.of_int (Unix.lseek fd 0 Unix.SEEK_CUR) in
        let ba =
          Bigarray.array1_of_genarray
            (Unix.map_file ~pos fd Bigarray.char Bigarray.c_layout false
               [| len |])
        in
        ignore (Unix.lseek fd len Unix.SEEK_CUR);
        ba
      in
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
      { read; read_ba = Some read_ba; seek; size; reset; custom_parser }
    in
    try
      let ans = f file in
      Unix.close fd;
      ans
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Unix.close fd;
      Printexc.raise_with_backtrace e bt

  let with_string ?custom_parser f s =
    let len = String.length s in
    let pos = ref 0 in
    let read b ofs n =
      let n = min (len - !pos) n in
      String.blit s !pos b ofs n;
      pos := !pos + n;
      n
    in
    let seek n = pos := !pos + n in
    let reset () = pos := 0 in
    let size () = Some len in
    f { read; read_ba = None; seek; size; reset; custom_parser }
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
