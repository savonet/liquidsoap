class dummy ~autostart ~on_start source =
  object (self)
    inherit
      Output.dummy
        ~autostart ~infallible:false ~register_telnet:false ~on_start
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

    method! get_frame =
      Frame.create ~length:(Lazy.force Frame.size) Frame.Fields.empty
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let started = ref false in
  let on_start () = started := true in
  let failed = new failed in
  let o = new dummy ~on_start ~autostart:true failed in
  let clock = Clock.clock ~start:false "source" in
  Clock.unify ~pos:o#pos o#clock (Clock.create_known clock);
  assert (not o#is_ready);
  o#content_type_computation_allowed;
  o#test_wake_up;
  assert (not !started);
  o#test_set_is_ready;
  assert o#is_ready;
  o#test_output;
  assert !started;
  ()
