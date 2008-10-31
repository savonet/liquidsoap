open Theora_encoded

(** Send Ogg theora through a shout connection *)

let proto = theora_proto @ Ogg_output_shout.proto


let () = 
  Lang.add_operator "output.icecast.theora"
    proto
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg Theora stream to an \
               Icecast-compatible.")
    (fun p -> 
       let e f v = f (List.assoc v p) in
       let quality = e Lang.to_int "quality" in
       let vorbis_quality = e Lang.to_float "vorbis_quality" in 
       let streams = 
         Theora_encoded.create_streams 
                ~quality ~vorbis_quality 
       in
       let bitrate = "Unknown" in
       ((new Ogg_output_shout.to_shout ~bitrate ~streams p):>Source.source))

