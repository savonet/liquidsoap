(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

module O = Avutil.Options

type arg = {
  name : string;
  typ : Lang.t;
  doc : string option;
  render : Lang.value -> Avfilter.args;
}

let float_text f =
  let exact p =
    let s = Printf.sprintf "%.*g" p f in
    if float_of_string s = f then Some s else None
  in
  match List.find_map exact [15; 16] with
    | Some s -> s
    | None -> Printf.sprintf "%.17g" f

let seconds_of_us us = Int64.to_float us /. 1_000_000.

let duration_text us =
  if Int64.rem us 1_000_000L = 0L then Int64.to_string (Int64.div us 1_000_000L)
  else float_text (seconds_of_us us)

let name_or_none = function Some name -> name | None -> "none"
let rational_text { Avutil.num; den } = Printf.sprintf "%d/%d" num den
let invalid value message = raise (Error.Invalid_value (value, message, []))

let ground_type : O.ground -> Lang.t = function
  | `Int _ | `Flags _ | `Int64 _ | `UInt64 _ | `Duration _ -> Lang.int_t
  | `Float _ | `Double _ -> Lang.float_t
  | `Bool _ -> Lang.bool_t
  | `Rational _ | `String _ | `Binary _ | `Dict _ | `Image_size _
  | `Video_rate _ | `Color _ | `Pixel_fmt _ | `Sample_fmt _ | `Channel_layout _
    ->
      Lang.string_t

let parse_rational value s =
  match String.split_on_char '/' s with
    | [num; den] -> (
        try { Avutil.num = int_of_string num; den = int_of_string den }
        with _ -> invalid value "Invalid rational: expected \"num/den\"")
    | _ -> invalid value "Invalid rational: expected \"num/den\""

(* A ground option, as the three facets of §5.2 and §5.3: the script value to
   its argument and its text, and the declared entry rendered as text. *)
type ground_view = {
  convert : Lang.value -> Avfilter.ground_arg * string;
  check_range : Lang.value -> unit;
  default : string option;
  values : (string * string) list;
  check_named : Lang.value -> unit;
  sentinel : Lang.value -> Avfilter.ground_arg option;
}

let range_check ~name ~cmp ~text (entry : _ O.entry) value v =
  (match entry.min with
    | Some min when cmp v min < 0 ->
        invalid value (Printf.sprintf "%s must be at least %s." name (text min))
    | _ -> ());
  match entry.max with
    | Some max when cmp v max > 0 ->
        invalid value (Printf.sprintf "%s must be at most %s." name (text max))
    | _ -> ()

let named_check ~name ~equal ~text (entry : _ O.entry) value v =
  if
    entry.values <> [] && entry.min = None && entry.max = None
    && not (List.exists (fun (_, n) -> equal n v) entry.values)
  then
    invalid value
      (Printf.sprintf "%s must be one of: %s." name
         (String.concat ", "
            (List.map
               (fun (label, n) -> Printf.sprintf "%s (%s)" (text n) label)
               entry.values)))

let view ~name (spec : O.ground) : ground_view =
  let generic (type a) ~(get : Lang.value -> a)
      ~(arg : a -> Avfilter.ground_arg) ~(text : a -> string) ~cmp
      ~(entry : a O.entry) =
    let equal a b = cmp a b = 0 in
    {
      convert =
        (fun value ->
          let v = get value in
          (arg v, text v));
      check_range =
        (fun value -> range_check ~name ~cmp ~text entry value (get value));
      default = Option.map text entry.default;
      values = List.map (fun (label, v) -> (label, text v)) entry.values;
      check_named =
        (fun value -> named_check ~name ~equal ~text entry value (get value));
      sentinel = (fun _ -> None);
    }
  in
  let int64_sentinel (entry : int64 O.entry) value =
    match (entry.default, Lang.to_int value) with
      | Some d, v when d = Int64.max_int && v = max_int ->
          Some (`Int64 Int64.max_int)
      | Some d, v when d = Int64.min_int && v = min_int ->
          Some (`Int64 Int64.min_int)
      | _ -> None
  in
  let string_view ~(entry : string O.entry) =
    {
      convert =
        (fun value ->
          let s = Lang.to_string value in
          (`String s, s));
      check_range = (fun _ -> ());
      default = entry.default;
      values = [];
      check_named = (fun _ -> ());
      sentinel = (fun _ -> None);
    }
  in
  match spec with
    | `Int entry ->
        generic ~get:Lang.to_int
          ~arg:(fun i -> `Int i)
          ~text:string_of_int ~cmp:compare ~entry
    | `Int64 entry | `UInt64 entry ->
        let v =
          generic
            ~get:(fun v -> Int64.of_int (Lang.to_int v))
            ~arg:(fun i -> `Int64 i)
            ~text:Int64.to_string ~cmp:compare ~entry
        in
        { v with sentinel = int64_sentinel entry }
    | `Flags entry ->
        let v =
          generic
            ~get:(fun v -> Int64.of_int (Lang.to_int v))
            ~arg:(fun i -> `Int64 i)
            ~text:Int64.to_string ~cmp:compare ~entry
        in
        let all =
          List.fold_left (fun m (_, f) -> Int64.logor m f) 0L entry.values
        in
        {
          v with
          check_named =
            (fun value ->
              let bits = Int64.of_int (Lang.to_int value) in
              if Int64.logand bits (Int64.lognot all) <> 0L then
                invalid value
                  (Printf.sprintf "%s must be a combination of: %s." name
                     (String.concat ", "
                        (List.map
                           (fun (label, f) -> Printf.sprintf "%Ld (%s)" f label)
                           entry.values))));
          sentinel = int64_sentinel entry;
        }
    | `Duration entry ->
        let get v = Int64.mul (Int64.of_int (Lang.to_int v)) 1_000_000L in
        let v =
          generic ~get
            ~arg:(fun us -> `String (duration_text us))
            ~text:duration_text ~cmp:compare ~entry
        in
        { v with sentinel = int64_sentinel entry }
    | `Float entry | `Double entry ->
        generic ~get:Lang.to_float
          ~arg:(fun f -> `String (float_text f))
          ~text:float_text ~cmp:compare ~entry
    | `Rational entry ->
        let get v = parse_rational v (Lang.to_string v) in
        let as_float { Avutil.num; den } = float num /. float den in
        generic ~get
          ~arg:(fun r -> `Rational r)
          ~text:rational_text
          ~cmp:(fun a b -> compare (as_float a) (as_float b))
          ~entry
    | `Bool entry ->
        generic ~get:Lang.to_bool
          ~arg:(fun b -> `String (string_of_bool b))
          ~text:string_of_bool ~cmp:compare ~entry
    | `String entry
    | `Binary entry
    | `Dict entry
    | `Image_size entry
    | `Video_rate entry
    | `Color entry ->
        string_view ~entry
    | `Pixel_fmt (entry : Avutil.Pixel_format.t O.entry) ->
        let get v =
          let s = Lang.to_string v in
          try Avutil.Pixel_format.of_string s
          with _ -> invalid v (Printf.sprintf "Unknown pixel format: %s" s)
        in
        let text p = name_or_none (Avutil.Pixel_format.to_string p) in
        {
          convert =
            (fun value ->
              let p = get value in
              (`String (text p), text p));
          check_range = (fun _ -> ());
          default = Option.map text entry.default;
          values = [];
          check_named = (fun _ -> ());
          sentinel = (fun _ -> None);
        }
    | `Sample_fmt entry ->
        let get v =
          let s = Lang.to_string v in
          try Avutil.Sample_format.find s
          with _ -> invalid v (Printf.sprintf "Unknown sample format: %s" s)
        in
        let text f = name_or_none (Avutil.Sample_format.get_name f) in
        {
          convert =
            (fun value ->
              let f = get value in
              (`String (text f), text f));
          check_range = (fun _ -> ());
          default = Option.map text entry.default;
          values = [];
          check_named = (fun _ -> ());
          sentinel = (fun _ -> None);
        }
    | `Channel_layout entry ->
        let get v =
          let s = Lang.to_string v in
          try Avutil.Channel_layout.find s
          with _ -> invalid v (Printf.sprintf "Unknown channel layout: %s" s)
        in
        let text = Avutil.Channel_layout.get_description in
        {
          convert =
            (fun value ->
              let l = get value in
              (`String (text l), text l));
          check_range = (fun _ -> ());
          default = Option.map text entry.default;
          values = [];
          check_named = (fun _ -> ());
          sentinel = (fun _ -> None);
        }

(* §5.3. *)
let doc ~help ~flags (view : ground_view) =
  let values =
    match view.values with
      | [] -> None
      | values ->
          let list =
            String.concat ", "
              (List.map
                 (fun (label, v) -> Printf.sprintf "%s (%s)" v label)
                 values)
          in
          Some (if flags then list ^ ", combined by addition" else list)
  in
  let default = Option.map (Printf.sprintf "default: %s") view.default in
  let possible = Option.map (Printf.sprintf "possible values: %s") values in
  match (help, List.filter_map Fun.id [default; possible]) with
    | Some help, [] -> Some help
    | Some help, parts ->
        Some (Printf.sprintf "%s. (%s)" help (String.concat ", " parts))
    | None, [] -> None
    | None, parts ->
        let s = String.concat ", " parts in
        Some (String.capitalize_ascii s)

let of_opt (opt : O.opt) =
  let name = opt.name in
  match opt.spec with
    | `Array ground ->
        let view = view ~name ground in
        {
          name;
          typ = Lang.list_t (ground_type ground);
          doc = opt.help;
          render =
            (fun value ->
              let texts =
                List.map
                  (fun element ->
                    view.check_range element;
                    `String (snd (view.convert element)))
                  (Lang.to_list value)
              in
              `Pair (name, `Array texts));
        }
    | #O.ground as ground ->
        let view = view ~name ground in
        {
          name;
          typ = ground_type ground;
          doc =
            doc ~help:opt.help
              ~flags:(match ground with `Flags _ -> true | _ -> false)
              view;
          render =
            (fun value ->
              let arg, _ = view.convert value in
              view.check_range value;
              view.check_named value;
              match view.sentinel value with
                | Some sentinel ->
                    `Pair (name, (sentinel :> Avfilter.valued_arg))
                | None -> `Pair (name, (arg :> Avfilter.valued_arg)));
        }

let of_options options =
  let seen = Hashtbl.create 10 in
  List.filter_map
    (fun (opt : O.opt) ->
      if Hashtbl.mem seen opt.name then None
      else (
        Hashtbl.replace seen opt.name ();
        Some (of_opt opt)))
    (O.opts options)

let of_filter (filter : [ `Unattached ] Avfilter.filter) =
  of_options filter.options

let render args values =
  List.filter_map
    (fun arg ->
      match Lang.to_option (List.assoc arg.name values) with
        | None -> None
        | Some value -> Some (arg.render value))
    args
