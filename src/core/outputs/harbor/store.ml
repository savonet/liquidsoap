(* Offset [o] lives at index [o mod capacity]. [horizon] is the lowest offset
   whose bytes are valid in [data]; it only grows, and the writer raises it
   before overwriting, so a reader that checks it after its copy sees any
   overwrite that touched its range. *)
type buffer = { data : bytes; horizon : int Atomic.t }

type t = {
  retain : int;
  base_capacity : int;
  current : buffer Atomic.t;
  tail : int Atomic.t;
}

let base_extra = 65536

let create ~retain () =
  let base_capacity = retain + base_extra in
  {
    retain;
    base_capacity;
    current =
      Atomic.make { data = Bytes.create base_capacity; horizon = Atomic.make 0 };
    tail = Atomic.make 0;
  }

let tail t = Atomic.get t.tail
let capacity t = Bytes.length (Atomic.get t.current).data

let blit_circular ~to_ring data offset flat flat_offset length =
  let capacity = Bytes.length data in
  let index = offset mod capacity in
  let first = min length (capacity - index) in
  let copy ring_index flat_index len =
    if to_ring then Bytes.blit flat flat_index data ring_index len
    else Bytes.blit data ring_index flat flat_index len
  in
  copy index flat_offset first;
  if first < length then copy 0 (flat_offset + first) (length - first)

let resize t ~tail ~capacity =
  let old = Atomic.get t.current in
  let start = max (Atomic.get old.horizon) (tail - t.retain) in
  let start = max start (tail - Bytes.length old.data) in
  let length = tail - start in
  let data = Bytes.create capacity in
  let window = Bytes.create length in
  blit_circular ~to_ring:false old.data start window 0 length;
  blit_circular ~to_ring:true data start window 0 length;
  Atomic.set t.current { data; horizon = Atomic.make start }

let append t src src_offset length =
  if length > 0 then (
    let tail = Atomic.get t.tail in
    let needed = t.retain + length in
    let current_capacity = capacity t in
    if needed > current_capacity then resize t ~tail ~capacity:needed
    else if
      current_capacity > t.base_capacity
      && current_capacity > 2 * max needed t.base_capacity
    then resize t ~tail ~capacity:(max needed t.base_capacity);
    let buffer = Atomic.get t.current in
    let overwritten = tail + length - Bytes.length buffer.data in
    if overwritten > Atomic.get buffer.horizon then
      Atomic.set buffer.horizon overwritten;
    blit_circular ~to_ring:true buffer.data tail src src_offset length;
    Atomic.set t.tail (tail + length))

let read t ~offset ~length dst dst_offset =
  let tail = Atomic.get t.tail in
  let buffer = Atomic.get t.current in
  if
    offset < 0 || length < 0
    || offset + length > tail
    || offset < Atomic.get buffer.horizon
  then false
  else (
    blit_circular ~to_ring:false buffer.data offset dst dst_offset length;
    offset >= Atomic.get buffer.horizon)
