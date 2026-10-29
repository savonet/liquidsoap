(* The shared stream store: exact bytes across the wrap, invalid ranges outside
   the window, retention across an append longer than the store, and no torn
   range reported valid under a concurrent writer. *)

module Store = Harbor_output.Store

let pattern offset = Char.chr (offset * 7 mod 251)

let append store length =
  let offset = Store.tail store in
  Store.append store
    (Bytes.init length (fun i -> pattern (offset + i)))
    0 length

let readable store ~offset ~length =
  let buffer = Bytes.create length in
  if Store.read store ~offset ~length buffer 0 then (
    for i = 0 to length - 1 do
      if Bytes.get buffer i <> pattern (offset + i) then
        failwith (Printf.sprintf "wrong byte at offset %d" (offset + i))
    done;
    true)
  else false

let round_trip_and_wrap () =
  let store = Store.create ~retain:1000 () in
  let capacity = Store.capacity store in
  let straddles () =
    let tail = Store.tail store in
    tail > 3 * capacity && tail mod capacity > 300 && tail mod capacity < 900
  in
  while not (straddles ()) do
    append store 77
  done;
  let tail = Store.tail store in
  assert (readable store ~offset:(tail - 1500) ~length:1500);
  assert (not (readable store ~offset:(tail - capacity - 10) ~length:10));
  assert (not (readable store ~offset:(tail - 10) ~length:20));
  let crossing = ref 0 in
  for offset = tail - 1000 to tail - 100 do
    assert (readable store ~offset ~length:100);
    if offset mod capacity > capacity - 100 then incr crossing
  done;
  assert (!crossing > 0)

let window_across_large_append () =
  let retain = 5000 in
  let store = Store.create ~retain () in
  append store 3000;
  append store 4000;
  let start = Store.tail store in
  append store (4 * Store.capacity store);
  assert (readable store ~offset:(start - retain) ~length:retain);
  assert (readable store ~offset:start ~length:(Store.tail store - start));
  let large = Store.capacity store in
  for _ = 1 to 10 do
    append store 2000
  done;
  assert (Store.capacity store < large);
  let tail = Store.tail store in
  assert (readable store ~offset:(tail - 2000 - retain) ~length:retain)

(* One writer appending every size up to past the store's capacity, one reader
   chasing ranges near the overwrite edge: every range reported valid holds
   exactly its pattern bytes. *)
let concurrent () =
  let store = Store.create ~retain:4096 () in
  let capacity = Store.capacity store in
  let stop = Atomic.make false in
  let valid = Atomic.make 0 and invalid = Atomic.make 0 in
  let reader =
    Domain.spawn (fun () ->
        let buffer = Bytes.create capacity in
        while not (Atomic.get stop) do
          let tail = Store.tail store in
          let length = 1 + Random.int (min capacity (max 1 tail)) in
          let offset = max 0 (tail - length - Random.int 64) in
          let length = min length (tail - offset) in
          if length > 0 then
            if Store.read store ~offset ~length buffer 0 then (
              for i = 0 to length - 1 do
                if Bytes.get buffer i <> pattern (offset + i) then
                  failwith
                    (Printf.sprintf "torn byte at offset %d reported valid"
                       (offset + i))
              done;
              Atomic.incr valid)
            else Atomic.incr invalid
        done)
  in
  for round = 1 to 20000 do
    append store (1 + (round * 131 mod (capacity + (capacity / 2))))
  done;
  Atomic.set stop true;
  Domain.join reader;
  Printf.printf "concurrent reads: %d valid, %d invalid\n" (Atomic.get valid)
    (Atomic.get invalid);
  assert (Atomic.get valid > 100);
  assert (Atomic.get invalid > 0)

let () =
  round_trip_and_wrap ();
  window_across_large_append ();
  concurrent ();
  print_endline "store checks passed"
