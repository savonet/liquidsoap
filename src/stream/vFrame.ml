include Frame

let vot = Fmt.video_frames_of_ticks
let tov = Fmt.ticks_of_video_frames

let size t = vot (size t)
let position t = vot (position t)
let add_break t i = add_break t (tov i)

let set_metadata t i m = set_metadata t (tov i) m
let get_metadata t i = get_metadata t (tov i)

let get_rgb b =
  let tracks = Array.to_list (Frame.get_tracks b) in
  let ans =
    List.fold_left
      (fun l t ->
         match t with
           | RGB a -> a::l
           | _ -> l
      ) [] tracks in
    Array.of_list ans
