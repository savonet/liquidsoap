(** What the language leaves to whoever links it. A hook has one implementation,
    and a process that has none may put a stand-in in its place. *)
type 'a t = { name : string; mutable fn : 'a; mutable implemented : bool }

let make name fn = { name; fn; implemented = false }
let get { fn } = fn

let implement hook fn =
  if hook.implemented then
    failwith (Printf.sprintf "Hook %s is already implemented!" hook.name);
  hook.fn <- fn;
  hook.implemented <- true

(** What to use where nobody implements the hook, such as a process that only
    typechecks scripts. *)
let fallback hook fn = if not hook.implemented then hook.fn <- fn

let type_of_encoder =
  make "type_of_encoder" (fun ~pos:_ _ ->
      failwith "Encoders are not implemented!")

type encoder_params =
  [ `Anonymous of string | `Encoder of encoder | `Labelled of string * Value.t ]
  list

and encoder = string * encoder_params

let make_encoder =
  make "make_encoder" (fun ~pos:_ _ -> failwith "Encoders are not implemented!")

let has_encoder = make "has_encoder" (fun _ -> false)
let liq_libs_dir = make "liq_libs_dir" (fun () -> raise Not_found)
let log_path = ref None

type log =
  < f : 'a. int -> ('a, unit, string, unit) format4 -> 'a
  ; critical : 'a. ('a, unit, string, unit) format4 -> 'a
  ; severe : 'a. ('a, unit, string, unit) format4 -> 'a
  ; important : 'a. ('a, unit, string, unit) format4 -> 'a
  ; info : 'a. ('a, unit, string, unit) format4 -> 'a
  ; debug : 'a. ('a, unit, string, unit) format4 -> 'a >

let make_log =
  make "make_log" (fun name ->
      let name = String.concat "." name in
      object (self : log)
        method f lvl =
          let time = Unix.gettimeofday () in
          Printf.ksprintf (fun s ->
              List.iter
                (Printf.printf "%f [%s:%d]: %s" time name lvl)
                (String.split_on_char '\n' s))

        method critical = self#f 1
        method severe = self#f 2
        method important = self#f 3
        method info = self#f 4
        method debug = self#f 5
      end)

let log name =
  object (self : log)
    method f lvl = (get make_log name)#f lvl
    method critical = self#f 1
    method severe = self#f 2
    method important = self#f 3
    method info = self#f 4
    method debug = self#f 5
  end

let eval_check = make "eval_check" (fun ~env:_ ~tm:_ _ -> ())
let mk_source_ty = make "mk_source_ty" (fun ?pos:_ _ _ -> assert false)
let mk_clock_ty = make "mk_clock_ty" (fun ?pos:_ () -> assert false)
let source_methods_t = make "source_methods_t" (fun _ -> assert false)
let getpwnam = Lang_string.getpwnam
