class dummy ~clock ~autostart source =
  object (self)
    inherit
      Output.dummy
        ~clock ~autostart ~infallible:false ~register_telnet:false
        (Lang.source (source :> Source.source))

    method test_wake_up = self#wake_up (self :> Clock.source)
    val mutable test_can_generate_frame = false
    method test_set_can_generate_frame = test_can_generate_frame <- true
    method! can_generate_frame = test_can_generate_frame
    method test_output = self#output
  end

class test_source =
  object (self)
    inherit Debug_sources.fail "test_source"
    val mutable test_can_generate_frame = false
    method test_set_can_generate_frame = test_can_generate_frame <- true
    method! can_generate_frame = test_can_generate_frame
    method! generate_frame = self#empty_frame
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let started = ref false in
  let test_source = new test_source in
  let clock = Clock.create ~sync:`Passive () in
  Clock.start ~force:true clock;
  let o = new dummy ~clock ~autostart:true test_source in
  o#on_start (fun () -> started := true);
  o#content_type_computation_allowed;
  assert (not o#can_generate_frame);
  let a = o#test_wake_up in
  assert (not !started);
  Clock.tick clock;
  o#test_set_can_generate_frame;
  assert o#is_ready;
  test_source#test_set_can_generate_frame;
  assert test_source#is_ready;
  o#test_output;
  assert !started;
  o#sleep a;
  ()
