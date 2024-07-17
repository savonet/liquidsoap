
type session
type mode = Recv | Send

type t = {
  session : session ;
  mutable t0 : float ;
  mutable t : int64 ;
}

external _new_session : mode -> string -> int -> session = "liquidsoap_new_session"

let new_session mode ip port = {
  session = _new_session mode ip port ;
  t0 = 0. ;
  t = 0L
}

external _send : session -> string -> unit = "liquidsoap_send_buffer"
external _recv : session -> string -> bool = "liquidsoap_recv_buffer"
external meta_recv : session -> string = "liquidsoap_recv_metadata"
external meta_send : session -> string -> unit = "liquidsoap_send_metadata"

let encode h =
  let encode s =
    (String.make 1 (char_of_int (String.length s)))^s
  in
  let buf = Hashtbl.fold (fun k v s -> s^(encode k)^(encode v)) h "" in
    if String.length buf > 64*1024 then "" else buf

let decode s =
  let n = String.length s in
  let rec to_list offset acc =
    if offset = n then acc else
      let size = int_of_char s.[offset] in
      let w = String.sub s (offset+1) size in
        to_list (offset+1+size) (w::acc)
  in
  let to_list n l = try to_list n l with _ -> [] in
  let t = Array.of_list (to_list 0 []) in
  let l = (Array.length t)/2 in
  let h = Hashtbl.create l in
    for i = 0 to l-1 do
      Hashtbl.add h t.(2*i+1) t.(2*i)
    done ;
    h

let samples_per_buffer =
  (Mixer.Buffer.size*8)/
  (Mixer.Buffer.format.Mixer.channels*Mixer.Buffer.format.Mixer.sample_size)
let delay =
  (float samples_per_buffer)/.
  (float Mixer.Buffer.format.Mixer.sample_freq)

let lastlog = ref (Unix.time ())
let sync t =
  if t.t = 0L then
    ( t.t <- 1L ;
      t.t0 <- Unix.gettimeofday () )
  else
    let d = (t.t0+.(Int64.to_float t.t)*.delay) -.
            (Unix.gettimeofday ()) in
      if d>0. then
        ignore (Unix.select [] [] [] d)
      else
        if Unix.time () -. !lastlog > 1. then
          ( lastlog := Unix.time () ;
            Printf.fprintf stderr "We must catchup %f seconds\n%!" (-.d) ) ;
      t.t <- Int64.add t.t 1L

let send t b =
  assert (not (Mixer.Buffer.is_partial b)) ;
  sync t ;
  begin match Mixer.Buffer.get_metadata b with
    | None -> ()
    | Some m ->
        meta_send t.session (encode m)
  end ;
  _send t.session (Mixer.Buffer.to_string b)

let recv t b =
  Mixer.Buffer.free_metadatas b ;
  sync t ;
  let s = meta_recv t.session in
    if s <> "" then
      begin
        let h = decode s in
          Hashtbl.iter
            (fun k v ->
               Printf.printf "%s=%S\n" k v) h ;
          Printf.printf " ---- \n%!" ;
          Mixer.Buffer.push_metadata b h
      end ;
    _recv t.session (Mixer.Buffer.to_string b)
