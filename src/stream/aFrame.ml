
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

let fill_frame gen frame =
  add_break frame
    (Float_pcm.Generator.fill
       gen (get_float_pcm frame) (position frame))

let fill_frame_from_raw gen frame =
  add_break frame
    (Float_pcm.Generator_from_raw.fill
       gen (get_float_pcm frame) (position frame))

let blankify b off len =
  Float_pcm.blankify (get_float_pcm b) off len

let multiply b = Float_pcm.multiply (get_float_pcm b)

let add b1 off1 b2 = Float_pcm.add (get_float_pcm b1) off1 (get_float_pcm b2)

let substract b1 off1 b2 off2 len =
  Float_pcm.substract (get_float_pcm b1) off1 (get_float_pcm b2) off2 len

let rms b = Float_pcm.rms (get_float_pcm b)
