type mark =
  | Split of { time : int; forced : bool }
  | Start of { time : int; first : bool }

type candidate = { offset : int; time : int; mutable waiting : string list }

type t = {
  buffer : Buffer.t;
  init_length : Buffer.t -> int -> int option;
  streams : (string * bool ref) list;
  (* Accepted, increasing offsets. *)
  mutable marks : (int * mark) list;
  (* Undecided, oldest first. *)
  mutable candidates : candidate list;
  (* Requests without their candidate yet, increasing. *)
  mutable requests : int list;
}

let create ~fields ~init_length () =
  {
    buffer = Buffer.create 4096;
    init_length;
    streams = List.map (fun f -> (f, ref true)) fields;
    marks = [];
    candidates = [];
    requests = [];
  }

let buffer t = t.buffer

let inter_coded t field =
  Option.iter (fun all_key -> all_key := false) (List.assoc_opt field t.streams)

let all_key t = List.for_all (fun (_, k) -> !k) t.streams

let request t time =
  match List.rev t.requests with
    | last :: _ when last >= time -> ()
    | _ -> t.requests <- t.requests @ [time]

let accept t c =
  t.marks <-
    List.stable_sort
      (fun (a, _) (b, _) -> compare a b)
      (t.marks @ [(c.offset, Split { time = c.time; forced = false })])

let decide t ~field ~key ~superseded =
  t.candidates <-
    List.filter_map
      (fun c ->
        if not (List.mem field c.waiting) then Some c
        else if not key then None
        else (
          c.waiting <- List.filter (( <> ) field) c.waiting;
          if c.waiting = [] then (
            if not superseded then accept t c;
            None)
          else Some c))
      t.candidates

let packet t ~flush ~field ~key ~time =
  match List.assoc_opt field t.streams with
    | None -> ()
    | Some all_key ->
        let at_request = List.exists (fun r -> r <= time) t.requests in
        let takes = key && ((not !all_key) || at_request) in
        decide t ~field ~key ~superseded:takes;
        if at_request then
          t.requests <- List.filter (fun r -> r > time) t.requests;
        if takes then (
          flush ();
          let waiting =
            List.filter_map
              (fun (f, k) -> if f <> field && not !k then Some f else None)
              t.streams
          in
          let c = { offset = Buffer.length t.buffer; time; waiting } in
          if waiting = [] then accept t c
          else t.candidates <- t.candidates @ [c]);
        if not key then all_key := false

let force t ~flush ~time =
  flush ();
  t.candidates <- [];
  t.marks <- t.marks @ [(Buffer.length t.buffer, Split { time; forced = true })]

let container_start t ~first ~time =
  t.candidates <- [];
  t.marks <- t.marks @ [(Buffer.length t.buffer, Start { time; first })]

let restart_pending t =
  List.exists (function _, Start _ -> true | _ -> false) t.marks

type taken = { init : string option option; pieces : Encoder.hls_piece list }

let take t =
  let limit =
    match t.candidates with c :: _ -> c.offset | [] -> Buffer.length t.buffer
  in
  let bytes pos offset acc =
    if offset > pos then
      Encoder.Bytes (Strings.of_string (Buffer.sub t.buffer pos (offset - pos)))
      :: acc
    else acc
  in
  let rec go ~init pos acc = function
    | (offset, Split { time; forced }) :: rest when offset <= limit ->
        go ~init offset
          (Encoder.Split { time; forced } :: bytes pos offset acc)
          rest
    | (offset, Start { time; first }) :: rest as marks when offset <= limit -> (
        match t.init_length t.buffer offset with
          | None -> (init, offset, bytes pos offset acc, marks)
          | Some length ->
              let segment =
                if length = 0 then None
                else Some (Buffer.sub t.buffer offset length)
              in
              let acc = bytes pos offset acc in
              if first then go ~init:(Some segment) (offset + length) acc rest
              else
                go ~init (offset + length)
                  (Encoder.Restart { time; init = segment } :: acc)
                  rest)
    | marks -> (init, limit, bytes pos limit acc, marks)
  in
  let init, consumed, pieces, marks = go ~init:None 0 [] t.marks in
  let rest = Buffer.sub t.buffer consumed (Buffer.length t.buffer - consumed) in
  Buffer.clear t.buffer;
  Buffer.add_string t.buffer rest;
  t.marks <- List.map (fun (o, m) -> (o - consumed, m)) marks;
  t.candidates <-
    List.map (fun c -> { c with offset = c.offset - consumed }) t.candidates;
  { init; pieces = List.rev pieces }
