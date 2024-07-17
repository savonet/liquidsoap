
(** Plug for outputing the scheduler's frames. Plug-ins providing
  * implementations of that are in [src/outputs]. *)

(** A plugin is given a loop, and should initialize itself, then
  * run the loop with the proper output function,
  * then process to its destroying (closing files, sockets, ...) *)
type loop = (Mixer.Buffer.t -> unit) -> unit
let plug : (string->loop->unit) Plug.plug = Plug.create "outputs"

open Dtools

exception Invalid_output_plugin of string

let get name =
  let plugin = Conf.get_string ~root:name ~default:"icecast2" "type" in
    match plug#get plugin with
      | None -> raise (Invalid_output_plugin plugin)
      | Some output -> output name

let from_conf () =
  (* Soaptube has a single output and uses that variable to find it. *)
  get (Conf.get_string ~default:"output" "output")

let list_from_conf () =
  (* Liquidsoap accepts multiple outputs, defined in that list. *)
  List.map get (Conf.get_list ~default:["output"] "outputs")
