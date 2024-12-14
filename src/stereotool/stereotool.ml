open Ctypes
open Foreign

type handler = unit ptr

let handler : handler typ = ptr void

module type Config = sig
  val filename : string
end

exception Library_not_found
exception Library_initialized of string

let strnlen = foreign "strnlen" (ocaml_bytes @-> int @-> returning int)

module C (Conf : Config) = struct
  let lib =
    try Dl.dlopen ~filename:Conf.filename ~flags:[Dl.RTLD_NOW]
    with _ -> raise Library_not_found

  let foreign = foreign ~from:lib

  let software_version =
    foreign "stereoTool_GetSoftwareVersion" (void @-> returning int)

  let api_version = foreign "stereoTool_GetApiVersion" (void @-> returning int)
  let create = foreign "stereoTool_Create" (string_opt @-> returning handler)
  let delete = foreign "stereoTool_Delete" (handler @-> returning void)

  let valid_license =
    foreign "stereoTool_CheckLicenseValid" (handler @-> returning bool)

  let unlincensed_used_features =
    foreign "stereoTool_GetUnlicensedUsedFeatures"
      (handler @-> ocaml_bytes @-> int @-> returning bool)

  let load_preset =
    foreign "stereoTool_LoadPreset"
      (handler @-> string @-> int @-> returning bool)

  let save_preset =
    foreign "stereoTool_SavePreset"
      (handler @-> string @-> int @-> returning bool)

  let reset = foreign "stereoTool_Reset" (handler @-> returning void)

  let latency =
    foreign "stereoTool_GetLatency2" (handler @-> int @-> bool @-> returning int)

  let process =
    foreign "stereoTool_Process"
      (handler @-> ptr float @-> int @-> int @-> int @-> returning void)
end

module type C = module type of C (struct
  let filename = "foo"
end)

type t = { handler : handler; _module : (module C) }

type load_type =
  [ `Totalinit
  | `All_settings
  | `Audiofm
  | `Audio
  | `Processing
  | `Repair
  | `Repair_no_pnr
  | `Sublevel_pnr ]

let int_of_load_type = function
  | `Totalinit -> 10387
  | `All_settings -> 10386
  | `Audiofm -> 10385
  | `Audio -> 10384
  | `Processing -> 10709
  | `Repair -> 10708
  | `Repair_no_pnr -> 11069
  | `Sublevel_pnr -> 10699

let initialized = Atomic.make None

let init ?license_key ~filename () =
  (match Atomic.get initialized with
    | Some f when f <> filename -> raise (Library_initialized f)
    | _ -> Atomic.set initialized (Some filename));
  try
    let module C = C (struct
      let filename = filename
    end) in
    let handler = C.create license_key in
    Gc.finalise C.delete handler;
    { handler; _module = (module C : C) }
  with _ -> raise Library_not_found

let api_version { _module; _ } =
  let module C = (val _module : C) in
  C.api_version ()

let software_version { _module; _ } =
  let module C = (val _module : C) in
  C.software_version ()

let valid_license { handler; _module } =
  let module C = (val _module : C) in
  C.valid_license handler

let unlincensed_used_features =
  let buflen = 1024 in
  let buf = Bytes.create buflen in
  fun { handler; _module } ->
    let module C = (val _module : C) in
    let ptr = ocaml_bytes_start buf in
    if C.unlincensed_used_features handler ptr buflen then (
      let n = strnlen ptr buflen in
      Some (Bytes.to_string (Bytes.sub buf 0 n)))
    else None

let load_preset ?(load_type = `Totalinit) ~filename { handler; _module } =
  let module C = (val _module : C) in
  C.load_preset handler filename (int_of_load_type load_type)

let latency ~samplerate ~feed_silence { handler; _module } =
  let module C = (val _module : C) in
  C.latency handler samplerate feed_silence

let process_interleaved ~samplerate ~channels { handler; _module } samples ofs
    len =
  let module C = (val _module : C) in
  let buf = CArray.make float len in
  for i = 0 to len - 1 do
    CArray.unsafe_set buf i samples.(ofs + i)
  done;
  C.process handler (CArray.start buf) len channels samplerate;
  for i = 0 to len - 1 do
    samples.(ofs + i) <- CArray.unsafe_get buf i
  done

let process ~samplerate { handler; _module } samples ofs len =
  let channels = Array.length samples in
  if 0 < channels && 0 < len then (
    let module C = (val _module : C) in
    let buf = CArray.make float (channels * len) in
    for c = 0 to channels - 1 do
      let chan = samples.(c) in
      for i = 0 to len - 1 do
        CArray.unsafe_set buf ((i * channels) + c) chan.(ofs + i)
      done
    done;
    C.process handler (CArray.start buf) len channels samplerate;
    for c = 0 to channels - 1 do
      let chan = samples.(c) in
      for i = 0 to len - 1 do
        chan.(ofs + i) <- CArray.unsafe_get buf ((i * channels) + c)
      done
    done)
