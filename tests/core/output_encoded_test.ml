let insert_metadata_called = ref false
let encode_called = ref false
let send_called = ref false

(* Make sure send is always called before insert metadata. *)

class encoded_test =
  object (self)
    inherit
      Output.encoded
        ~output_kind:"foo" ~name:"encoded_test" ~infallible:false
          ~register_telnet:false
        ~on_start:(fun _ -> ())
        ~on_stop:(fun _ -> ())
        ~autostart:false
        (Lang.source (new Noise.noise None))

    method insert_metadata _ =
      assert !send_called;
      insert_metadata_called := true

    method encode _ _ _ =
      encode_called := true;
      ()

    method send _ =
      assert !encode_called;
      send_called := true

    method test_send_frame frame = self#send_frame frame
    method start = ()
    method stop = ()
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let encoded_test = new encoded_test in
  let frame = Frame.dummy () in
  Frame.add_break frame (Lazy.force Frame.size);
  let m = Frame.Metadata.from_list [("foo", "bla")] in
  Frame.set_metadata frame 0 m;
  encoded_test#test_send_frame frame;
  assert !insert_metadata_called
