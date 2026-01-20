class start_stop_test _state start stop =
  object
    inherit Start_stop.base
    method! state = _state
    method start = start ()
    method stop = stop ()
    method fallible = true
    method on_before_streaming_cycle fn = fn ()
  end

exception Success

let () =
  let s =
    new start_stop_test `Idle (fun () -> assert false) (fun () -> assert false)
  in
  s#reset;
  let s =
    new start_stop_test
      `Stopped
      (fun () -> assert false)
      (fun () -> assert false)
  in
  s#reset;
  let s =
    new start_stop_test `Started (fun () -> raise Success) (fun () -> ())
  in
  try
    s#reset;
    assert false
  with Success -> ()
