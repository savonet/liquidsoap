include Liquidsoap_lang.Doc

(** Documentation for protocols. *)
module Protocol = struct
  type t = { name : string; description : string; syntax : string }

  let db = ref []

  let add ~name ~doc ~syntax =
    let p = { name; description = doc; syntax } in
    db := p :: !db

  let db () = List.sort compare !db
  let count () = db () |> List.length

  let print_md print =
    List.iter
      (fun p ->
        Printf.ksprintf print "### %s\n\n%s\n\nThe syntax is `%s`.\n\n" p.name
          p.description p.syntax)
      (db ())
end
