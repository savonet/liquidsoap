class dummy ~autostart ~on_start source =
  object (self)
    inherit
      Output.dummy
        ~autostart ~infallible:false ~register_telnet:false ~on_start
        ~on_stop:(fun () -> ())
        (Lang.source (source :> Source.source))

    method test_wake_up = self#wake_up
    val mutable test_can_generate_frame = false
    method test_set_can_generate_frame = test_can_generate_frame <- true
    method! can_generate_frame = test_can_generate_frame
    method test_output = self#output
  end

class failed =
  object
    inherit Debug_sources.fail "failed"
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let started = ref false in
  let on_start () = started := true in
  let failed = new failed in
  let o = new dummy ~on_start ~autostart:true failed in
  Clock.start ~sync:`Passive o#clock;
  o#content_type_computation_allowed;
  assert (not o#can_generate_frame);
  o#test_wake_up;
  assert (not !started);
  o#test_set_can_generate_frame;
  assert o#can_generate_frame;
  o#test_output;
  assert !started;
  ()
