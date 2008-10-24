open Source
open Dtools
open Theora

let create_encoder ~quality =
  let os = Ogg.Stream.create () in
  let frame_x = Fmt.video_width () in
  let frame_y = Fmt.video_height () in
  (* Theora has a divisible-by-sixteen restriction for the encoded video size. *)
  (* Scale the frame size up to the nearest /16 and calculate offsets. *)
  let video_x = ((frame_x + 15) lsr 4) lsl 4 in
  let video_y = ((frame_y + 15) lsr 4) lsl 4 in
  let frame_x_offset = ((video_x - frame_x) / 2) land (lnot 1) in
  let frame_y_offset = ((video_y - frame_y) / 2) land (lnot 1) in
  let video_r = 100000 in
  let info =
    {
      width = video_x;
      height = video_y;
      frame_width = frame_x;
      frame_height = frame_y;
      offset_x = frame_x_offset;
      offset_y = frame_y_offset;
      fps_numerator = Fmt.video_frames_of_seconds 1.;
      fps_denominator = 1;
      aspect_numerator = 1;
      aspect_denominator = 1;
      colorspace = CS_unspecified;
      target_bitrate = video_r;
      quality = quality;
      quick_p = true;
      version_major = 0;
      version_minor = 0;
      version_subminor = 0;
      dropframes_p = false;
      keyframe_auto_p = true;
      keyframe_frequency = 64;
      keyframe_frequency_force = 64;
      keyframe_data_target_bitrate = (video_r * 3 / 2);
      keyframe_auto_threshold = 80;
      keyframe_mindistance = 8;
      noise_sensitivity = 1;
      sharpness = 1; (* ??? *)
      pixelformat = PF_420
    }
  in
  let enc = Encoder.create info in
    os, enc

(** Output in a ogg theora *)

class virtual base ~quality ~vorbis_quality =
  let ((y,y_stride), (u, v, uv_stride) as yuv) = 
    RGB.create_yuv (Fmt.video_width ()) (Fmt.video_height ()) 
  in
  let theora_yuv = 
  {
    Theora.y_width = Fmt.video_width ();
    Theora.y_height = Fmt.video_height ();
    Theora.y_stride = y_stride;
    Theora.uv_width = Fmt.video_width () / 2;
    Theora.uv_height = Fmt.video_height () / 2;
    Theora.uv_stride = uv_stride;
    Theora.y = y;
    Theora.u = u;
    Theora.v = v;
  }
  in
  let convert = 
    Video_converter.find_converter 
      (Video_converter.RGB Video_converter.Rgba_32)
      (Video_converter.YUV Video_converter.Yuvj_420)
  in
  (* Basic vorbis encoder. TODO: modular ogg encoding/multiplexing..*)
  let create_vorbis_encoder () = 
    let os = Ogg.Stream.create () in
    let channels = Fmt.channels () in
    let freq = Fmt.samples_of_seconds 1. in
    let quality = vorbis_quality in
    os,Vorbis.Encoder.create_vbr channels freq quality
  in 
object (self)

  val virtual mutable encoder : ((Vorbis.Encoder.t option)*Theora.Encoder.t) option
  val mutable os = None
  val mutable vos = None
  val flush = Buffer.create 1024

  method new_encoder m =
    (* Quick and dirty work around until we have theora EOS.. *)
    if encoder = None then
     begin
      let oggs, enc = create_encoder ~quality in
        os <- Some oggs;
        Encoder.encode_header enc oggs;
        assert(Buffer.length flush = 0);
        Buffer.add_string flush (Ogg.Stream.pageout oggs);
        let vorbis_enc = 
          if vorbis_quality > 0. then
           begin
            let voggs, venc = create_vorbis_encoder () in 
            let tags = Vorbis.tags () in
            Vorbis.Encoder.headerout venc voggs tags ;
            vos <- Some voggs;
            Buffer.add_string flush (Ogg.Stream.pageout voggs);
            Some venc
          end
         else
            None
        in
        let add a b c = (a,b)::c in
        let comments = 
          Hashtbl.fold add m [] 
        in
        Encoder.encode_comments oggs comments;
        Encoder.encode_tables enc oggs;
        encoder <- Some (vorbis_enc,enc);
        Buffer.add_string flush (Ogg.Stream.flush oggs)
    end;

  method end_of_os = 
    (* TODO: theora eos ! 
    begin
     match encoder with
       |Some (Some venc,_) -> 
           let voggs = Utils.get_some vos in 
           Vorbis.Encoder.end_of_stream venc voggs;
           Buffer.add_string flush (Ogg.Stream.flush voggs)
       | _ -> ()
     end;
     let b = Buffer.contents flush in
     Buffer.reset flush;
     b *)
    ""

  method reset_encoder (_:(Vorbis.Encoder.t option)*Theora.Encoder.t) 
                        (m:(string,string) Hashtbl.t) =
    (** TODO: theora EOS ! 
    let b = self#end_of_os in
    self#new_encoder m;
    b*)
    ""

  method encode (vencoder,encoder) frame start stop =
    let os = Utils.get_some os in
    let vid = VFrame.get_rgb frame in
    let vid = vid.(0) in (* TODO: handle multiple chans *)
      for i = Fmt.video_frames_of_ticks start to 
              Fmt.video_frames_of_ticks stop - 1 
      do
        convert 
         (Video_converter.frame_of_internal_rgb vid.(i)) 
         (Video_converter.frame_of_internal_yuv 
           (Fmt.video_width ()) 
           (Fmt.video_height ())
           yuv); (* TODO: custom video size.. *)
        Encoder.encode_buffer encoder os theora_yuv
      done;
      Buffer.add_string flush (Ogg.Stream.pagesout os);
    begin
     match vencoder with
       | Some venc -> 
          let voggs = Utils.get_some vos in
          let buf = AFrame.get_float_pcm frame in
          let start = Fmt.samples_of_ticks start in
          let stop = Fmt.samples_of_ticks stop in
          Vorbis.Encoder.encode_buffer_float venc voggs buf start stop;
          Buffer.add_string flush (Ogg.Stream.pagesout voggs);
       | None -> ()
    end;
    let s = Buffer.contents flush in
    Buffer.reset flush;
    s

  method output_reset = ()
end

class to_file
  ~append ~perm ~dir_perm
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~filename ~vorbis_quality ~quality ~autostart source =
object (self)
  inherit
    [(Vorbis.Encoder.t option)*Theora.Encoder.t] Output.encoded
         ~name:filename ~kind:"output.file" ~autostart source
  inherit File_output.to_file
            ~reload_delay ~reload_predicate ~reload_on_metadata
            ~append ~perm ~dir_perm filename as to_file
  inherit base 
         ~quality ~vorbis_quality as base

  method reset_encoder enc m =
    to_file#on_reset_encoder ;
    to_file#set_metadata (Hashtbl.find (Hashtbl.copy m)) ;
    base#reset_encoder enc m

  method output_start =
      base#new_encoder (Hashtbl.create 10) ;
      to_file#file_output_start

  method output_stop =
    let f = base#end_of_os in
    to_file#send f ;
    to_file#file_output_stop

  method output_reset = ()
end

let () =
  Lang.add_operator "output.file.theora"
    ([
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output threads on operator initialization." ;

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
      @ File_output.proto @ ["", Lang.source_t, None, None ])
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

