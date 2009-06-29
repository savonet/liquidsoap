include Frame

let tracks f =
  let tracks = Array.to_list (Frame.get_tracks f) in
  let ans =
    List.fold_left
      (fun l t ->
         match t with
           | Midi m -> m::l
           | _ -> l
      ) [] tracks in
    Array.of_list ans

let set_events f e = ()
