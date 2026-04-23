let make_flags () =
  let encode_metadata_called = ref false in
  let encode_called = ref false in
  let send_called = ref false in
  (encode_metadata_called, encode_called, send_called)

(* Default: metadata_first. encode_metadata called before encode, encode before send. *)

class metadata_first_test encode_metadata_called encode_called send_called =
  object (self)
    inherit
      [unit] Output.encoded
        ~output_kind:"foo" ~name:"encoded_test" ~infallible:false
          ~register_telnet:false ~autostart:false ~export_cover_metadata:false
        (Lang.source (new Noise.noise None))

    method self_sync = (`Static, None)

    method encode_metadata _ =
      assert (not !encode_called);
      encode_metadata_called := true

    method encode _ =
      assert !encode_metadata_called;
      encode_called := true;
      ()

    method send _ =
      assert !encode_called;
      send_called := true

    method test_send_frame frame = self#send_frame frame
    method start = ()
    method stop = ()
  end

(* Data_first: encode and send before encode_metadata (e.g. icecast with ICY). *)

class data_first_test encode_metadata_called encode_called send_called =
  object (self)
    inherit
      [unit] Output.encoded
        ~output_kind:"foo" ~name:"encoded_test" ~infallible:false
          ~register_telnet:false ~autostart:false ~export_cover_metadata:false
        (Lang.source (new Noise.noise None))

    initializer self#set_encoding_order `Data_first
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

let make_frame encoded_test =
  let frame =
    Frame.create ~length:(Lazy.force Frame.size) encoded_test#content_type
  in
  let m = Frame.Metadata.from_list [("foo", "bla")] in
  Frame.add_metadata frame 0 m

let () =
  Frame_settings.lazy_config_eval := true;
  let encode_metadata_called, encode_called, send_called = make_flags () in
  let t =
    new metadata_first_test encode_metadata_called encode_called send_called
  in
  t#content_type_computation_allowed;
  t#test_send_frame (make_frame t);
  assert !encode_metadata_called;
  let encode_metadata_called, encode_called, send_called = make_flags () in
  let t =
    new data_first_test encode_metadata_called encode_called send_called
  in
  t#content_type_computation_allowed;
  t#test_send_frame (make_frame t);
  assert !encode_metadata_called
