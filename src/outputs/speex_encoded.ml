(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source

exception Internal

let speex_proto = [
  "samplerate",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some ("Output sample rate. Use liquidsoap's default if <= 0.");

  "bitrate",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some "Target bitrate (in kbps). Not used if <= 0.";

  "quality",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some "Target quality (0 to 10). Not used if <= 0.";

  "mode",
  Lang.string_t,
  Some (Lang.string "narrowband"),
  Some "Encoding mode, one of \"narrowband\", \"wideband\" \
        or \"ultra-wideband\".";

  "stereo",
  Lang.bool_t,
  Some (Lang.bool false),
  None;

  "vbr",
  Lang.bool_t,
  Some (Lang.bool true),
  Some "Encode in vbr mode.";

  "frames_per_packet",
  Lang.int_t,
  Some (Lang.int 1),
  Some "Number of frame per Ogg packet (1 to 10).";

  "complexity",
  Lang.int_t,
  Some (Lang.int 3),
  Some "Encoding complexity (0-10).";

  "abr",
  Lang.int_t,
  Some (Lang.int (-1)),
  Some "Set average bitrate. Not used if <= 0.";
]

class virtual base freq stereo mode bitrate vbr fpp complexity abr quality =
  let samples_per_second = float (Fmt.samples_per_second()) in
  let freq = 
    if freq > 0 then
      freq
    else
      int_of_float samples_per_second
  in
  let stereo = 
    if Fmt.channels () = 1 then
      false
    else
      stereo
  in
  let remaining_init =
    if stereo then
     [|[||];[||]|]
    else
     [|[||]|]
  in
object (self)
  val tmp =
    if stereo then
      [||]
    else
      Float_pcm.create_buffer 1 (Fmt.samples_per_frame())

  val mutable _os = None
  val mutable flush = false
  val mutable frame_size = None
  val mutable remaining = remaining_init

  method virtual set_encoder : Speex.Encoder.t -> unit

  method new_encoder stereo =
    let enc =
      Speex.Encoder.init mode fpp
    in
      if bitrate > 0 then
        Speex.Encoder.set enc Speex.SPEEX_SET_BITRATE bitrate;
      Speex.Encoder.set enc Speex.SPEEX_SET_COMPLEXITY complexity;
      if abr > 0 then
        Speex.Encoder.set enc Speex.SPEEX_SET_ABR abr;
      Speex.Encoder.set enc Speex.SPEEX_SET_SAMPLING_RATE freq;
      let ivbr = 
        if vbr then 1 else 0
      in
      Speex.Encoder.set enc Speex.SPEEX_SET_VBR ivbr;
      if quality > 0 then
        if vbr then
          Speex.Encoder.set enc Speex.SPEEX_SET_VBR_QUALITY quality
        else
          Speex.Encoder.set enc Speex.SPEEX_SET_QUALITY quality;
      frame_size <- Some (Speex.Encoder.get enc Speex.SPEEX_GET_FRAME_SIZE);
      self#set_encoder enc ;
      enc

  method new_os ?(tags=None) enc = 
    let os = Ogg.Stream.create () in
    let chans = if stereo then 2 else 1 in
    let header = Speex.Header.init ~rate:freq ~nb_channels:chans ~bitrate 
                                   ~mode ~vbr ~frames_per_packet:fpp ()
    in
    let tags = 
      match tags with
        | None -> ["artist","The Savonet Team";
                   "title", "Liquidsoap Stream"]
        | Some t -> t
    in
    Speex.Header.encode_header header tags os;
    (* Must flush headers first.. *)
    flush <- true ;
    _os <- Some os

  method end_of_os encoder = 
    match _os with
      | None -> ""
      | Some os ->
         let encoder = Utils.get_some encoder in
         Speex.Encoder.eos encoder os ;
         let f = Ogg.Stream.flush os in
         _os <- None ;
         f

  method get_os e = 
    match _os with
      | Some s -> s
      | None -> self#new_os e ;
                Utils.get_some _os

  method reset_encoder encoder m =
    let rec get l l' =
      match l with
        | k :: r ->
          begin 
            try
              get r ((k,Hashtbl.find m k) :: l')
            with _ -> get r l'
          end
        | [] -> l'
    in
    let title =
      try
        Hashtbl.find m "title"
      with
        | _ -> 
        begin
          try
            let s = Hashtbl.find m "uri" in
            let title = Filename.basename s in
                (try
                   String.sub title 0 (String.rindex title '.')
                 with
                   | Not_found -> title)
          with
            | _ -> "Unknown"
        end
    in
    let l' = ["title",title] in
    let tags = Some (get ["artist";"genre";"date";
                          "album";"tracknumber";"comment"]
                         l')
    in
        let flushed = self#end_of_os (Some encoder) in
        let encoder = self#new_encoder stereo in
        self#new_os ~tags encoder; 
        flushed

  method encode e frame start len =
    let b = AFrame.get_float_pcm frame in
    let start = Fmt.samples_of_ticks start in
    let len = Fmt.samples_of_ticks len in
    let frame_size = Utils.get_some frame_size in
    let b =
      if stereo then b else begin
        for i = start to start+len-1 do
	  let n = Fmt.channels () in
	  let f i = 
	    Array.fold_left (fun x y -> x +. y.(i)) 0. b
	  in
          tmp.(0).(i) <- f i /. (float_of_int n)
        done ;
        tmp
      end
    in
    let buf =
      if float freq <> samples_per_second then
        Float_pcm.resample
          (float freq /. samples_per_second)
          b start len
      else
        b
    in
    let os = self#get_os e in
      let f = 
        if flush then
        ( flush <- false;
	  Ogg.Stream.flush os )
	else
	  ""
      in
      let buf = 
        if stereo then
          [|Array.append remaining.(0) buf.(0);
            Array.append remaining.(1) buf.(1)|] 
        else
          [|Array.append remaining.(0) buf.(0)|]
      in
      let len = Array.length buf.(0) in
      let encoded = ref f in
      let status = ref 0 in
      let feed () =
        let n = !status in
        if frame_size*n + frame_size < len then
        ( status := n + 1;
          (* Speex float API are values in - 32768. <= x <= 32767. ..
             I don't really trust this, it must be a bug, 
             so using the int API. *)
          let f x = 
            let x = int_of_float x in
            max (-32768) (min 32767 x)
          in
          let f x = Array.map  (fun x -> f (32767.*.x)) x in
          if stereo then
            [| f (Array.sub buf.(0) (frame_size*n) frame_size);
               f (Array.sub buf.(1) (frame_size*n) frame_size) |]
          else
            [| f (Array.sub buf.(0) (frame_size*n) frame_size) |] )
        else
          raise Internal
      in
      try
        while true  do
          let (h,v) = 
            if stereo then
              Speex.Encoder.encode_page_int_stereo e os feed
            else
              let feed () = 
                let x = feed () in
                x.(0)
              in
              Speex.Encoder.encode_page_int e os feed
          in
          encoded := !encoded ^ h ^ v;
        done;
        !encoded
      with 
        | Internal ->
            let n = !status in
            remaining <-
              if frame_size*n < len then 
                if stereo then
                  [|Array.sub buf.(0) (frame_size*n) (len - frame_size*n);
                    Array.sub buf.(1) (frame_size*n) (len - frame_size*n)|]
                else
                  [|Array.sub buf.(0) (frame_size*n) (len - frame_size*n)|]
              else
                remaining_init;
            !encoded

end

(** Output in an Ogg/speex file. *)
class to_file
  filename ~append ~perm ~dir_perm ~vbr ~fpp ~quality
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~mode ~bitrate ~complexity ~abr freq stereo source autostart =
object (self)
  inherit
    [Speex.Encoder.t] Output.encoded
      ~name:filename ~kind:"output.file" ~autostart source
  inherit File_output.to_file
            ~reload_delay ~reload_predicate ~reload_on_metadata
            ~append ~perm ~dir_perm filename as to_file
  inherit base freq stereo mode bitrate vbr fpp complexity abr quality as base

  method set_encoder e = encoder <- Some e

  method reset_encoder enc m =
    to_file#on_reset_encoder ;
    to_file#set_metadata (Hashtbl.find (Hashtbl.copy m)) ;
    base#reset_encoder enc m

  method output_start = 
    ignore(self#new_encoder stereo) ;
    to_file#file_output_start 

  method output_stop =
    let f = base#end_of_os encoder in
    to_file#send f ;
    to_file#file_output_stop 

end

let () =
  Lang.add_operator "output.file.speex"
    (speex_proto @ File_output.proto @ [
      "start",
      Lang.bool_t, Some (Lang.bool true),
      Some "Start output on operator initialization." ;

      "", Lang.source_t, None, None ])
    ~category:Lang.Output
    ~descr:("Output the source stream as an Ogg speex file.")
    (fun p ->
       let e f v = f (List.assoc v p) in
       let autostart = e Lang.to_bool "start" in
       let stereo = e Lang.to_bool "stereo" in
       let bitrate = (e Lang.to_int "bitrate") in
       let bitrate = 
         if bitrate > 0 then
           bitrate * 1000
         else
           bitrate
       in
       let vbr = e Lang.to_bool "vbr" in
       let fpp = e Lang.to_int "frames_per_packet" in
       let freq = e Lang.to_int "samplerate" in
       let quality = e Lang.to_int "quality" in
       let abr = (e Lang.to_int "abr") * 1000 in
       let complexity = e Lang.to_int "complexity" in
       let mode = 
         match e Lang.to_string "mode" with
           | "narrowband" -> Speex.Narrowband
           | "wideband" -> Speex.Wideband
           | "ultra-wideband" -> Speex.Ultra_wideband
           | _ -> failwith "Unknown speex mode"
       in
       let name = Lang.to_string (Lang.assoc "" 1 p) in
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
       let source = Lang.assoc "" 2 p in
         ((new to_file
             name ~append ~perm ~dir_perm ~quality
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~mode ~bitrate ~vbr ~fpp ~complexity ~abr
             freq stereo source autostart):>source))

