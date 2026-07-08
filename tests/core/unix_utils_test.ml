(* poll must report a closed fd as ready in the set it was registered under
   (POLLNVAL): if it were silently dropped, callers polling that fd would get
   an immediate return with no ready fd and busy-loop. *)

let () =
  (* Closed fd registered for write. *)
  let r, w = Unix.pipe () in
  Unix.close w;
  let _, ws, _ = Unix_utils.poll [] [w] [] 0.1 in
  assert (ws = [w]);
  Unix.close r;

  (* Closed fd registered for read. *)
  let r, w = Unix.pipe () in
  Unix.close r;
  let rs, _, _ = Unix_utils.poll [r] [] [] 0.1 in
  assert (rs = [r]);
  Unix.close w;

  (* Peer-closed pipe write end must be write-ready (POLLHUP path). *)
  let r, w = Unix.pipe () in
  Unix.close r;
  let _, ws, _ = Unix_utils.poll [] [w] [] 0.1 in
  assert (ws = [w]);
  Unix.close w
