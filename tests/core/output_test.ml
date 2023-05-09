class dummy ~autostart ~on_start source =
  object (self)
    inherit
      Output.dummy
        ~autostart ~infallible:false ~on_start
        ~on_stop:(fun () -> ())
        (Lang.source (source :> Source.source))

    method test_wake_up = self#wake_up []
    val mutable test_is_ready = false
    method test_set_is_ready = test_is_ready <- true
    method! is_ready = test_is_ready
    method test_output = self#output
  end

class failed =
  object
    inherit Debug_sources.fail "failed"

    method! get frame =
      Frame.add_break frame (SyncLazy.force Frame.size);
      Printf.printf "Frame pos: %d\n%!" (Frame.position frame);
      assert (not (Frame.is_partial frame))
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let started = ref false in
  let on_start () = started := true in
  let failed = new failed in
  let o = new dummy ~on_start ~autostart:true failed in
  assert (not o#is_ready);
  o#content_type_computation_allowed;
  o#test_wake_up;
  assert (not !started);
  o#test_set_is_ready;
  assert o#is_ready;
  o#test_output;
  assert !started;
  ()
