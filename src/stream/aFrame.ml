
include Frame

(* For now AFrame assumes that the audio frames all have the same format,
 * given by Fmt. *)

let sot = Fmt.samples_of_ticks
let tos = Fmt.ticks_of_samples

let size t = sot (size t)
let position t = sot (position t)
let breaks t = List.map sot (breaks t)
let add_break t i = add_break t (tos i)
let set_breaks t l = set_breaks t (List.map tos l)

let set_metadata t i m = set_metadata t (tos i) m
let get_metadata t i = get_metadata t (tos i)
let get_all_metadata t =
  List.map (fun (x,y) -> sot x, y) (get_all_metadata t)
let set_all_metadata t l =
  set_all_metadata t (List.map (fun (x,y) -> tos x, y) l)

(** Helper *)
let get_float_pcm b =
  let tracks = Array.to_list (Frame.get_tracks b) in
  let ans =
    List.fold_left
      (fun l t ->
         match t with
           | Float_pcm (_,a) -> a::l
           | _ -> l
      ) [] tracks in
    Array.of_list ans

let get_s16le_length b =
  let pcm = get_float_pcm b in
    2 * Array.length pcm * (Array.length pcm.(0))

let to_s16le b =
  let fpcm = get_float_pcm b in
  let slen = get_s16le_length b in
  let s = String.create slen in
    assert (Float_pcm.to_s16le fpcm 0 (Array.length fpcm.(0)) s 0 = slen);
    s

let fill_frame_main ?(add_break=true) f g h gen frame =
  let pos = position frame in
  let len = size frame in
  let offset = len - pos in
  (** First add new metadata and breaks. *)
  let old_meta = get_all_metadata frame in
  let new_meta = g gen offset in
  let sort l = 
    List.sort
      (fun x -> fun y -> y - x)
      l
  in
  let new_breaks = sort (h gen offset) in
  let old_breaks = breaks frame in
  let size = 
    match new_breaks with
      | a::_  -> Some a
      | _ -> None
  in
  (* Now fill the frame, advancing metadata and breaks *)
  let npos = Fmt.ticks_of_samples 
    (f gen ?size (get_float_pcm frame) pos) 
  in
  let pos = Fmt.ticks_of_samples pos in
  let breaks =
    let new_breaks = List.map (fun x -> x + pos) new_breaks in
    let l = List.filter (fun x -> x <= npos) (sort (old_breaks@new_breaks)) in
    if add_break then
      npos::l
    else
      l
  in
  set_breaks frame breaks;
  let new_meta = List.map (fun (x,y) -> (x + pos,y)) new_meta in
  let meta = List.filter (fun x -> fst(x) < npos) (old_meta@new_meta) in
  set_all_metadata frame meta

let fill_frame ?add_break = 
  fill_frame_main ?add_break
                  Float_pcm.Generator.fill 
                  Float_pcm.Generator.peek_metadata
                  Float_pcm.Generator.peek_breaks

let fill_frame_from_raw ?add_break = 
  fill_frame_main ?add_break
                  Float_pcm.Generator_from_raw.fill 
                  Float_pcm.Generator_from_raw.peek_metadata
                  Float_pcm.Generator_from_raw.peek_breaks

let feed_frame abg ?(sample_freq = Fmt.samples_per_second()) frame = 
  let meta = 
    List.filter (fun (x,_) -> x >= 0) (get_all_metadata frame) 
  in
  List.iter (Float_pcm.Generator.add_metadata abg) meta;
  let breaks = breaks frame in
  let max = Fmt.ticks_of_samples (size frame) in
  List.iter 
    (fun x -> if x < max && x >= 0 then
      Float_pcm.Generator.add_break abg x)
    breaks;
  let data = 
    Array.map 
     (fun x -> Array.sub x 0 (position frame))
     (get_float_pcm frame) 
  in
  Float_pcm.Generator.feed abg ~sample_freq data

let blankify b off len =
  Float_pcm.blankify (get_float_pcm b) off len

let multiply b = Float_pcm.multiply (get_float_pcm b)

let add b1 off1 b2 = Float_pcm.add (get_float_pcm b1) off1 (get_float_pcm b2)

let substract b1 off1 b2 off2 len =
  Float_pcm.substract (get_float_pcm b1) off1 (get_float_pcm b2) off2 len

let rms b = Float_pcm.rms (get_float_pcm b)
