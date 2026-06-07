open Portaudio
open Bigarray

let samplerate = ref 44100.

let choose_device () =
  print_endline "-1 Quit";
  let dcount = get_device_count () in
  for d = 0 to dcount - 1 do
    let dinfo = get_device_info d in
    Printf.printf "%d\t%s\n" d dinfo.d_name
  done;
  read_int ()

let formats = [| "int8"; "int16"; "int24"; "int32"; "float32" |]

let int_of_format s =
  let ans = ref (-1) in
  for i = 0 to Array.length formats - 1 do
    if s = formats.(i) then ans := i
  done;
  if !ans = -1 then raise Not_found;
  !ans

let choose_format () =
  for i = 0 to Array.length formats - 1 do
    let s = formats.(i) in
    Printf.printf "%d\t%s\n" i s
  done;
  read_int ()

let rec choose_interleaved () =
  print_endline "0 Non-interleaved";
  print_endline "1 Interleaved";
  match read_int () with 0 -> false | 1 -> true | _ -> choose_interleaved ()

let choose_callback () =
  print_endline "0 blocking write";
  print_endline "1 callback";
  read_int ()

let test_array stream init randf randv =
  print_endline "Testing arrays...";
  let buf = Array.make 256 init in
  let bbuf = [| buf; buf |] in
  for _ = 0 to 100 do
    for i = 0 to 255 do
      let rand = randf randv in
      buf.(i) <- rand
    done;
    Portaudio.write_stream stream bbuf 0 256
  done

let fill_ba ba inter randf randv =
  for i = 0 to 255 do
    let rand = randf randv in
    let left, right =
      if inter then ([| 2 * i |], [| (2 * i) + 1 |])
      else ([| 0; i |], [| 1; i |])
    in
    Genarray.set ba left rand;
    Genarray.set ba right rand
  done

let test_bigarray stream inter batype randf randv =
  print_endline "Testing Bigarrays...";
  let dims = if inter then [| 2 * 256 |] else [| 2; 256 |] in
  let ba = Genarray.create batype c_layout dims in
  for _ = 0 to 100 do
    fill_ba ba inter randf randv;
    Portaudio.write_stream_ba stream ba 0 256
  done

let start inter d fmt =
  match fmt with
    | 0 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int8;
              latency = 1.;
            }
        in
        let stream =
          open_stream ~interleaved:inter None outparam !samplerate 256 []
        in
        start_stream stream;
        test_array stream 0 Random.int 256;
        test_bigarray stream inter int8_signed Random.int 256;
        close_stream stream
    | 1 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int16;
              latency = 1.;
            }
        in
        let stream =
          open_stream ~interleaved:inter None outparam !samplerate 256 []
        in
        start_stream stream;
        test_array stream 0 Random.int 65536;
        test_bigarray stream inter int16_signed Random.int 65536;
        close_stream stream
    | 2 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int24;
              latency = 1.;
            }
        in
        let stream =
          open_stream ~interleaved:inter None outparam !samplerate 256 []
        in
        start_stream stream;
        test_array stream Int32.zero Random.int32 (Int32.of_int (4096 * 4096));
        test_bigarray stream inter int32 Random.int32
          (Int32.of_int (4096 * 4096));
        close_stream stream
    | 3 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int32;
              latency = 1.;
            }
        in
        let stream =
          open_stream ~interleaved:inter None outparam !samplerate 256 []
        in
        start_stream stream;
        test_array stream Int32.zero Random.int32 Int32.max_int;
        test_bigarray stream inter int32 Random.int32 Int32.max_int;
        close_stream stream
    | 4 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_float32;
              latency = 1.;
            }
        in
        let stream =
          open_stream ~interleaved:inter None outparam !samplerate 256 []
        in
        start_stream stream;
        test_array stream 0. (fun () -> 1. -. Random.float 2.) ();
        test_bigarray stream inter float32 (fun () -> 1. -. Random.float 2.) ();
        close_stream stream
    | _ -> ()

let start_callback d fmt =
  let cb r _ y l =
    for i = 0 to l - 1 do
      Genarray.set y [| 2 * i |] (r ());
      Genarray.set y [| (2 * i) + 1 |] (r ())
    done;
    0
  in
  match fmt with
    | 0 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int8;
              latency = 1.;
            }
        in
        let r () = Random.int 256 in
        let stream =
          open_stream ~callback:(cb r) None outparam !samplerate 0 []
        in
        start_stream stream;
        sleep 5000;
        close_stream stream
    | 1 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int16;
              latency = 1.;
            }
        in
        let r () = Random.int 65536 in
        let stream =
          open_stream ~callback:(cb r) None outparam !samplerate 0 []
        in
        start_stream stream;
        sleep 5000;
        close_stream stream
    | 2 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int24;
              latency = 1.;
            }
        in
        let r () = Random.int32 (Int32.of_int (4096 * 4096)) in
        let stream =
          open_stream ~callback:(cb r) None outparam !samplerate 0 []
        in
        start_stream stream;
        sleep 5000;
        close_stream stream
    | 3 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_int32;
              latency = 1.;
            }
        in
        let r () = Random.int32 Int32.max_int in
        let stream =
          open_stream ~callback:(cb r) None outparam !samplerate 0 []
        in
        start_stream stream;
        sleep 5000;
        close_stream stream
    | 4 ->
        let outparam =
          Some
            {
              channels = 2;
              device = d;
              sample_format = format_float32;
              latency = 1.;
            }
        in
        let r () = 1. -. Random.float 2. in
        let stream =
          open_stream ~callback:(cb r) None outparam !samplerate 0 []
        in
        start_stream stream;
        sleep 5000;
        close_stream stream
    | _ -> ()

let device = ref None
let format = ref None
let callback = ref None
let interleaved = ref None

let main () =
  let d = match !device with Some d -> d | None -> choose_device () in
  if d = -1 then exit 0;
  let fmt = match !format with Some f -> f | None -> choose_format () in
  let cb = match !callback with Some c -> c | None -> choose_callback () in
  match cb with
    | 0 ->
        let inter =
          match !interleaved with Some i -> i | None -> choose_interleaved ()
        in
        start inter d fmt
    | 1 -> start_callback d fmt
    | _ -> ()

let () =
  Arg.parse
    [
      ("--device", Int (fun n -> device := Some n), "Device.");
      ("--format", String (fun s -> format := Some (int_of_format s)), "Format.");
      ("--blocking", Unit (fun () -> callback := Some 0), "Use blocking mode.");
      ("--callback", Unit (fun () -> callback := Some 1), "Use callbacks.");
      ( "--interleaved",
        Unit (fun () -> interleaved := Some true),
        "Interleaved samples." );
      ( "--non-interleaved",
        Unit (fun () -> interleaved := Some false),
        "Non-interleaved samples." );
      ("--samplerate", Int (fun f -> samplerate := float f), "Samplerate");
    ]
    (fun s -> Printf.eprintf "Ignored argument: %s\n%!" s)
    "make noise";
  Printf.printf "Using %s.\n%!" (get_version_string ());
  Random.self_init ();
  Portaudio.init ();
  main ()
