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

(* Pairs in parameter-table order, values normalised: integers as integers,
   [num/den] reduced. *)
type pairs = (string * string) list

let positive_int value =
  match int_of_string_opt value with
    | Some n when n > 0 && String.for_all (fun c -> c >= '0' && c <= '9') value
      ->
        Some (string_of_int n)
    | _ -> None

let rational value =
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  match String.split_on_char '/' value with
    | [num; den] -> (
        match (positive_int num, positive_int den) with
          | Some num, Some den ->
              let num = int_of_string num and den = int_of_string den in
              let g = gcd num den in
              Some (Printf.sprintf "%d/%d" (num / g) (den / g))
          | _ -> None)
    | _ -> None

let one_of names value = if List.mem value names then Some value else None

module Pairs (L : sig
  val labels : (string * (string -> string option)) list
end) =
struct
  type t = pairs

  let default = []

  let sort pairs =
    List.filter_map
      (fun (label, _) ->
        Option.map (fun v -> (label, v)) (List.assoc_opt label pairs))
      L.labels

  let to_string pairs =
    String.concat "," (List.map (fun (l, v) -> l ^ "=" ^ v) (sort pairs))

  let parse_param label value =
    match List.assoc_opt label L.labels with
      | Some check -> Option.map (fun v -> [(label, v)]) (check value)
      | None -> None

  let conflict a b =
    List.find_opt
      (fun (label, v) ->
        match List.assoc_opt label b with Some v' -> v <> v' | None -> false)
      a

  let compatible a b = conflict a b = None

  let merge a b =
    match conflict a b with
      | Some (label, _) -> failwith (Printf.sprintf "Incompatible %s" label)
      | None -> sort (a @ List.filter (fun (l, _) -> not (List.mem_assoc l a)) b)

  let serialize = to_string

  let parse_serialized = function
    | "" -> Some []
    | s -> (
        try
          Some
            (List.fold_left
               (fun acc item ->
                 match String.index_opt item '=' with
                   | None -> raise Exit
                   | Some i -> (
                       let label = String.sub item 0 i in
                       let value =
                         String.sub item (i + 1) (String.length item - i - 1)
                       in
                       match parse_param label value with
                         | Some p when compatible acc p -> merge acc p
                         | _ -> raise Exit))
               []
               (String.split_on_char ',' s))
        with Exit -> None)
end

module Params = struct
  let name = "stand-in"

  module Copy = Pairs (struct
    let labels = []
  end)

  module Audio = Pairs (struct
    let labels =
      [
        ("channel_layout", one_of Ffmpeg_names.channel_layouts);
        ("sample_format", one_of Ffmpeg_names.sample_formats);
        ("sample_rate", positive_int);
      ]
  end)

  module Video = Pairs (struct
    let labels =
      [
        ("width", positive_int);
        ("height", positive_int);
        ("pixel_format", one_of Ffmpeg_names.pixel_formats);
        ("pixel_aspect", rational);
      ]
  end)
end

include Liquidsoap_ffmpeg_formats.Kinds.Make (Params)
