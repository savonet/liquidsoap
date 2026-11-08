let bitrate bytes duration = int_of_float (float_of_int (bytes * 8) /. duration)

let peak_bitrate ~target segments =
  let target = float_of_int target in
  let rec runs acc = function
    | [] -> acc
    | _ :: rest as l ->
        let rec extend acc ~bytes ~duration = function
          | [] -> acc
          | s :: rest ->
              let bytes = bytes + s.Window.size
              and duration = duration +. s.Window.duration in
              let acc =
                if
                  duration >= 0.5 *. target
                  && duration <= 1.5 *. target
                  && duration > 0.
                then max acc (Some (bitrate bytes duration))
                else acc
              in
              if duration > 1.5 *. target then acc
              else extend acc ~bytes ~duration rest
        in
        runs (extend acc ~bytes:0 ~duration:0. l) rest
  in
  match segments with
    | [s] when s.Window.duration > 0. -> Some (bitrate s.size s.duration)
    | _ -> runs None segments

let codecs segments =
  try
    let formats =
      List.concat_map
        (fun s ->
          match s.Window.codecs with
            | Some c -> List.map String.trim (String.split_on_char ',' c)
            | None -> raise Exit)
        segments
    in
    let unique =
      List.fold_left
        (fun acc f -> if List.mem f acc then acc else acc @ [f])
        [] formats
    in
    if unique = [] then None else Some (String.concat "," unique)
  with Exit -> None
