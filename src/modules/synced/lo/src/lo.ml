exception Error
exception Unhandled

let () =
  Callback.register_exception "lo_exn_error" Error;
  Callback.register_exception "lo_exn_unhandled" Unhandled

module Address = struct
  type t

  external create : string -> string -> t = "caml_lo_address_new"

  let create host port = create host (string_of_int port)

  external default : unit -> t = "caml_lo_address_default"
end

module Message = struct
  type t
  type timetag = int * int

  type data =
    [ `Int32 of int
    | `Float of float
    | `String of string
    | `Blob of string
    | `Int64 of int
    | `Timetag of timetag
    | `Double of float
    | `Symbol of string
    | `Char of char
    | `Midi of string
    | `True
    | `False
    | `Nil
    | `Infinitum ]

  external create : unit -> t = "caml_lo_message_new"
  external add : t -> data -> unit = "caml_lo_message_add"

  let add_list m d = List.iter (add m) d

  external send : Address.t -> string -> t -> unit = "ocaml_lo_send_message"

  let to_string (x : data) =
    match x with
      | `Int32 n -> string_of_int n
      | `Int64 n -> string_of_int n
      | `Float f -> string_of_float f
      | `Double f -> string_of_float f
      | `Blob b -> b
      | `Timetag (t1, t2) -> Printf.sprintf "(%d,%d)" t1 t2
      | `String s -> s
      | `Symbol s -> s
      | `Char c -> String.make 1 c
      | `Midi _ -> "<MIDI>"
      | `True -> "true"
      | `False -> "false"
      | `Nil -> "nil"
      | `Infinitum -> "inf"
end

let send addr path data =
  let m = Message.create () in
  Message.add_list m data;
  Message.send addr path m

module Server = struct
  type t

  exception Stopped

  let () = Callback.register_exception "lo_exn_stopped" Stopped

  external create : string -> (string -> Message.data array -> unit) -> t
    = "caml_lo_server_new"

  let create p h = create (string_of_int p) h

  external recv : t -> unit = "caml_lo_server_recv"
  external stop : t -> unit = "caml_lo_server_free"
end
