open Avutil

let () =
  List.iter
    (fun layout ->
      Printf.printf "Channel layout: %s\n%!"
        (Channel_layout.get_description layout))
    Channel_layout.standard_layouts
