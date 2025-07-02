let encode_metadata_called = ref false
let encode_called = ref false
let send_called = ref false

(* Make sure send is always called before insert metadata. *)

class encoded_test =
  object (self)
    inherit
      [unit] Output.encoded
        ~output_kind:"foo" ~name:"encoded_test" ~infallible:false
          ~register_telnet:false
        ~on_start:(fun _ -> ())
        ~on_stop:(fun _ -> ())
        ~autostart:false ~export_cover_metadata:false
        (Lang.source (new Noise.noise None))

    method self_sync = (`Static, None)

    method encode_metadata _ =
      assert !send_called;
      encode_metadata_called := true

    method encode _ =
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
  encoded_test#content_type_computation_allowed;
  let frame =
    Frame.create ~length:(Lazy.force Frame.size) encoded_test#content_type
  in
  let m = Frame.Metadata.from_list [("foo", "bla")] in
  let frame = Frame.add_metadata frame 0 m in
  encoded_test#test_send_frame frame;
  assert !encode_metadata_called
