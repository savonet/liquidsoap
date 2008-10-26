open Theora_encoded

(** Send Ogg theora through a shout connection *)

let no_mount = "Use [name].ogg"
let no_name = "Use [mount]"

let proto =
  theora_proto @ Icecast2.proto @
  [ "mount", Lang.string_t, Some (Lang.string no_mount), None ;
    "name", Lang.string_t, Some (Lang.string no_name), None ;
    "", Lang.source_t, None, None ]

class to_shout p =

  let e f v = f (List.assoc v p) in
  let s v = e Lang.to_string v in

  let name = s  "name" in
  let mount = s "mount" in
  let name =
    if name = no_name then
      if mount = no_mount then
        raise (Lang.Invalid_value
                 ((List.assoc "mount" p),
                  "Either name or mount must be defined"))
      else
        mount
    else
      name
  in
  let mount =
    if mount = no_mount then name ^ ".ogg" else mount
  in

  let quality = e Lang.to_int "quality" in
  let vorbis_quality = e Lang.to_float "vorbis_quality" in
  let source = List.assoc "" p in

object (self)
  inherit [Ogg_encoder.t] Icecast2.output
    (** TODO: check this parameter.. *)
    ~bitrate:(Printf.sprintf "Quality %i" quality) 
    ~mount ~name ~source p as super
  inherit Ogg_output.base as ogg
  inherit base ~quality ~vorbis_quality as base

  method output_start =
    ogg#output_start;
    base#new_encoder [];
    super#output_start

  method output_stop =
    let b = ogg#end_of_stream in
    ogg#output_stop;
    super#send b;
    super#output_stop
end

let () = 
  Lang.add_operator "output.icecast.theora"
    proto
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Theora stream to an \
               Icecast-compatible.")
    (fun p -> ((new to_shout p):>Source.source))

