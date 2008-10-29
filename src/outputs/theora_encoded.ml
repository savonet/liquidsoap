open Source
open Dtools
open Theora

(** Output in a ogg theora *)

class virtual base ~quality ~vorbis_quality =
  (* Basic vorbis encoder. TODO: modular ogg encoding/multiplexing..*)
  let create_vorbis_encoder () = 
    let channels = Fmt.channels () in
    let freq = Fmt.samples_of_seconds 1. in
    let quality = vorbis_quality in
    Vorbis_format.create channels freq quality []
  in 
object (self)

  val virtual mutable encoder : (Ogg_encoder.t) option

  val mutable vorbis_id = None
  val mutable theora_id = None

  method new_encoder m =
    let enc = Theora_format.create_encoder ~quality m in
    let ogg_enc = Utils.get_some encoder in
    theora_id <- Some (Ogg_encoder.register_track ogg_enc enc);
    if vorbis_quality > 0. then
     begin
      let enc = create_vorbis_encoder () in
      vorbis_id <- Some (Ogg_encoder.register_track ogg_enc enc)
     end;
    Ogg_encoder.streams_start ogg_enc

  method reset_encoder m =
    let enc = Utils.get_some encoder in
    begin
      match theora_id with
        | Some id -> 
           Ogg_encoder.end_of_track enc id;
           theora_id <- None
        | None -> ()
    end;
    if vorbis_quality > 0. then
     begin
      match vorbis_id with
        | Some id -> 
             Ogg_encoder.end_of_track enc id;
             vorbis_id <- None
        | None -> ()
     end;
    let flushed = Ogg_encoder.flush enc in
    let f x y l = 
      (x,y)::l
    in
    let tags = Hashtbl.fold f m [] in
    self#new_encoder tags;
    flushed

  method encode frame ofs len =
    if theora_id = None then
      self#new_encoder [];
    let vid = VFrame.get_rgb frame in
    let vofs = Fmt.video_frames_of_ticks ofs in
    let vlen = Fmt.video_frames_of_ticks len in
    let data = 
     Ogg_encoder.Video_data
      { 
       Ogg_encoder.
        data    = vid;
        offset  = vofs;
        length  = vlen
      }
    in
    let enc = Utils.get_some encoder in
    let id = Utils.get_some theora_id in
    Ogg_encoder.encode enc id data;
    begin
     match vorbis_id with
       | Some id -> 
          let buf = AFrame.get_float_pcm frame in
          let ofs = Fmt.samples_of_ticks ofs in
          let len = Fmt.samples_of_ticks len in
          let data = 
           Ogg_encoder.Audio_data
            { 
             Ogg_encoder.
              data   = buf;
              offset = ofs;
              length = len
            }
          in
          Ogg_encoder.encode enc id data;
       | None -> ()
    end;
    Ogg_encoder.get_data enc

  method output_reset = ()
end

class to_file
  ~append ~perm ~dir_perm
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~filename ~vorbis_quality ~quality ~autostart source =
object (self)
  inherit
    [Ogg_encoder.t] Output.encoded
         ~name:filename ~kind:"output.file" ~autostart source
  inherit File_output.to_file
            ~reload_delay ~reload_predicate ~reload_on_metadata
            ~append ~perm ~dir_perm filename as to_file
  inherit Ogg_output.base as ogg
  inherit base 
         ~quality ~vorbis_quality as base

  method reset_encoder m =
    to_file#on_reset_encoder ;
    to_file#set_metadata (Hashtbl.find (Hashtbl.copy m)) ;
    base#reset_encoder m

  method output_start =
    ogg#output_start;
    to_file#file_output_start

  method output_stop =
    let f = ogg#end_of_stream in
    ogg#output_stop;
    to_file#send f ;
    to_file#file_output_stop

  method output_reset = ()
end

let theora_proto = 
     [
      "quality",
      Lang.int_t,
      Some (Lang.int 100),
      Some "Quality setting for theora encoding." ;

      "vorbis_quality",
      Lang.float_t,
      Some (Lang.float 2.),
      Some "Quality setting for vorbis encoding. \
            Don't encode audio if value is negative or null." ;

     ]


let () =
  Lang.add_operator "output.file.theora"
    ([       
        "start",
         Lang.bool_t, Some (Lang.bool true),
         Some "Start output threads on operator initialization." ] @ 
      theora_proto @ File_output.proto @ ["", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:"Output the source's stream as an ogg/theora file."
    (fun p ->
       let e f v = f (List.assoc v p) in
       let quality = e Lang.to_int "quality" in
       let vorbis_quality = e Lang.to_float "vorbis_quality" in
       let autostart = e Lang.to_bool "start" in
       let filename = Lang.to_string (Lang.assoc "" 1 p) in
       let source = Lang.assoc "" 2 p in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
         ((new to_file ~filename
             ~append ~perm ~dir_perm
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~quality ~vorbis_quality ~autostart source):>source))

