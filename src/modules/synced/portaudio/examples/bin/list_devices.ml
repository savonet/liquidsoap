let () =
  Portaudio.init ();
  let c = Portaudio.get_device_count () in
  Printf.printf "Portaudio has %d devices.\n%!" c;
  let rec f p =
    if p < c then (
      let {
        Portaudio.d_struct_version;
        d_name;
        d_host_api;
        d_max_input_channels;
        d_max_output_channels;
        d_default_low_input_latency;
        d_default_low_output_latency;
        d_default_high_input_latency;
        d_default_high_output_latency;
        d_default_sample_rate;
      } =
        Portaudio.get_device_info p
      in
      Printf.printf
        {|
Device %d info:
- struct version: %d
- name: %s
- host API: %d
- max input channels: %d
- max output channels: %d
- default low input latency: %.02f
- default low output latency: %.02f
- default high input latency: %.02f
- default high output latency: %.02f
- default sample rate: %.02f
|}
        p d_struct_version d_name d_host_api d_max_input_channels
        d_max_output_channels d_default_low_input_latency
        d_default_low_output_latency d_default_high_input_latency
        d_default_high_output_latency d_default_sample_rate;
      f (p + 1))
  in
  f 0
