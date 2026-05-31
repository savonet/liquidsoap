open Mm_audio
open Mm_midi
module FFT = Audio.Mono.Analyze.FFT

let polyphony = 1
let mchan = 0
let oss_out = true

let list_diff cmp l1 l2 =
  List.fold_left
    (fun acc x -> if not (List.exists (cmp x) l2) then x :: acc else acc)
    [] l1

let rec list_head_n n l =
  if n = 0 then []
  else (match l with h :: t -> h :: list_head_n (n - 1) t | [] -> [])

let () =
  let fname = Sys.argv.(1) in
  let f = new Mm_mad.reader_of_file fname in
  let oss = new Mm_oss.writer f#channels f#sample_rate in
  let wav =
    new Audio.IO.Writer.to_wav_file f#channels f#sample_rate "out.wav"
  in
  let mid = new MIDI.IO.Writer.to_file f#sample_rate "out.mid" in
  let fft_n = 11 in
  let fft = FFT.init fft_n in
  let blen = FFT.length fft in
  let buf = Audio.create f#channels blen in
  let agc =
    Audio.Effect.auto_gain_control f#channels f#sample_rate ~kup:0.9 ~kdown:0.7
      ~rms_target:2. ()
  in
  let adsr =
    Audio.Mono.Effect.ADSR.make f#sample_rate (0.02, 0.01, 0.9, 0.05)
  in
  let synth = new Synth.saw ~adsr f#sample_rate in
  let loop = ref true in
  let prevnotes = ref [] in
  synth#set_volume 0.1;
  while !loop do
    let r = f#read buf 0 blen in
    agc#process buf 0 blen;
    loop := r <> 0;
    let notes =
      FFT.notes f#sample_rate fft ~note_min:(Audio.Note.create 0 4)
        ~volume_min:0.01 ~filter_harmonics:false (Audio.to_mono buf 0 blen)
    in
    let notes =
      List.sort (fun (_, v1) (_, v2) -> if v1 < v2 then 1 else -1) notes
    in
    let notes = list_head_n polyphony notes in
    (* Printf.printf "Notes: %d\n%!" (List.length notes); *)
    let ncmp (n1, _) (n2, _) = n1 = n2 in
    List.iter
      (fun (n, v) ->
        synth#note_off n 1.;
        mid#note_off mchan n (10. *. v))
      (list_diff ncmp !prevnotes notes);
    List.iter
      (fun (n, v) ->
        synth#note_on n 1.;
        mid#note_on mchan n (10. *. v))
      (list_diff ncmp notes !prevnotes);
    prevnotes := notes;
    synth#fill_add buf 0 blen;
    mid#advance blen;
    Audio.amplify 2. buf 0 blen;
    wav#write buf 0 blen;
    if oss_out then oss#write buf 0 blen
  done;
  wav#close;
  mid#close;
  oss#close;
  f#close
