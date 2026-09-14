let read_string ring ~ofs len =
  let b = Bytes.create len in
  if ByteRing.read ring ~ofs b 0 len then Some (Bytes.to_string b) else None

(* Round trip across the wrap point. *)
let () =
  let ring = ByteRing.create ~capacity:8 in
  assert (ByteRing.tail ring = 0);
  ByteRing.append ring (Strings.of_string "abcde");
  ByteRing.append ring (Strings.of_list ["fg"; "hij"]);
  assert (ByteRing.tail ring = 10);
  assert (read_string ring ~ofs:2 8 = Some "cdefghij");
  assert (read_string ring ~ofs:6 4 = Some "ghij");
  assert (read_string ring ~ofs:9 1 = Some "j");
  assert (read_string ring ~ofs:10 0 = Some "")

(* Bytes older than the capacity, or not yet written, are not available. *)
let () =
  let ring = ByteRing.create ~capacity:8 in
  ByteRing.append ring (Strings.of_string "01234");
  ByteRing.append ring (Strings.of_string "56789");
  assert (ByteRing.capacity ring = 8);
  assert (read_string ring ~ofs:1 2 = None);
  assert (read_string ring ~ofs:2 2 = Some "23");
  assert (read_string ring ~ofs:9 2 = None)

(* A chunk larger than the capacity grows the ring and keeps the retained
   bytes readable at their offsets. *)
let () =
  let ring = ByteRing.create ~capacity:4 in
  ByteRing.append ring (Strings.of_string "abcdef");
  assert (ByteRing.capacity ring = 12);
  assert (read_string ring ~ofs:2 4 = Some "cdef");
  ByteRing.append ring (Strings.of_string "ghijklmnopqrstuvwxyz");
  assert (ByteRing.capacity ring = 40);
  assert (read_string ring ~ofs:2 24 = Some "cdefghijklmnopqrstuvwxyz")

(* A writer thread and a reader thread: every byte the reader gets is the
   byte the writer put at that offset, and a lagging reader is told so. *)
let () =
  let ring = ByteRing.create ~capacity:1024 in
  let total = 200_000 in
  let byte_at ofs = Char.chr (ofs mod 251) in
  let writer =
    Thread.create
      (fun () ->
        let ofs = ref 0 in
        while !ofs < total do
          let len = min (1 + (!ofs mod 97)) (total - !ofs) in
          ByteRing.append ring
            (Strings.of_string (String.init len (fun i -> byte_at (!ofs + i))));
          ofs := !ofs + len;
          if !ofs mod 5000 < len then Thread.yield ()
        done)
      ()
  in
  let ofs = ref 0 in
  let torn = ref 0 in
  let buf = Bytes.create 64 in
  while !ofs < total do
    let tail = ByteRing.tail ring in
    if tail > !ofs then begin
      let len = min 64 (tail - !ofs) in
      if ByteRing.read ring ~ofs:!ofs buf 0 len then begin
        for i = 0 to len - 1 do
          assert (Bytes.get buf i = byte_at (!ofs + i))
        done;
        ofs := !ofs + len
      end
      else begin
        incr torn;
        ofs := max !ofs (ByteRing.tail ring - 512)
      end
    end
    else Thread.yield ()
  done;
  Thread.join writer;
  assert (ByteRing.tail ring = total)
