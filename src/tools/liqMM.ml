let ts m f =
  try
    Mutex.lock m;
    let ans = f () in
    Mutex.unlock m;
    ans
  with e ->
    Mutex.unlock m;
    raise e

module RingbufferTS = struct
  open Audio.Ringbuffer

  type ts_t = t * Mutex.t
  type t = ts_t

  let create chans size = (create chans size, Mutex.create ())
  let read_space (t, m) = ts m (fun () -> read_space t)
  let write_space (t, m) = ts m (fun () -> write_space t)
  let read_advance (t, m) n = ts m (fun () -> read_advance t n)
  let write_advance (t, m) n = ts m (fun () -> write_advance t n)
  let read (t, m) buf off len = ts m (fun () -> read t (Audio.sub buf off len))

  let write (t, m) buf off len =
    ts m (fun () -> write t (Audio.sub buf off len))

  let transmit (t, m) f = ts m (fun () -> transmit t f)
end
