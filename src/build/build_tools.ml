let read_files ~location dir =
  List.sort Stdlib.compare
    (Array.to_list (Sys.readdir (Filename.concat location dir)))
