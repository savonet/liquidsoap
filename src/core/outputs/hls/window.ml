type segment = {
  position : int;
  discontinuous : bool;
  discontinuity : int;
  forced : bool;
  codecs : string option;
  init : string option;
  name : string;
  tags : string list;
  duration : float;
  size : int;
  mutable listed_max : float;
  mutable left_at : float option;
}

type t = {
  mutable next_position : int;
  mutable next_init_position : int;
  mutable discontinuity_count : int;
  mutable segments : segment list;
  mutable window_start : int;
  mutable closed : float;
}

let create () =
  {
    next_position = 1;
    next_init_position = 0;
    discontinuity_count = 0;
    segments = [];
    window_start = 1;
    closed = 0.;
  }

let listed t = List.filter (fun s -> s.position >= t.window_start) t.segments
let duration l = List.fold_left (fun d s -> d +. s.duration) 0. l
let media_sequence t = match listed t with s :: _ -> s.position - 1 | [] -> 0

let discontinuity_sequence t =
  match listed t with s :: _ -> s.discontinuity | [] -> 0

let next_init_position t =
  let n = t.next_init_position in
  t.next_init_position <- n + 1;
  n

let publish t ~discontinuous ~forced ~codecs ~init ~name ~tags ~duration ~size =
  let position = t.next_position in
  t.next_position <- position + 1;
  if discontinuous then t.discontinuity_count <- t.discontinuity_count + 1;
  let s =
    {
      position;
      discontinuous;
      discontinuity = t.discontinuity_count;
      forced;
      codecs;
      init;
      name;
      tags;
      duration;
      size;
      listed_max = 0.;
      left_at = None;
    }
  in
  t.segments <- t.segments @ [s];
  t.closed <- t.closed +. duration;
  s

let slide t ~max_segments ~target =
  let rec slide = function
    | oldest :: (next :: _ as rest)
      when List.length rest >= max_segments
           && duration rest >= 3. *. float_of_int target ->
        oldest.left_at <- Some t.closed;
        t.window_start <- next.position;
        slide rest
    | _ -> ()
  in
  slide (listed t)

let mark_listed t =
  let l = listed t in
  let d = duration l in
  List.iter (fun s -> s.listed_max <- Float.max s.listed_max d) l

let available t s =
  match s.left_at with
    | None -> true
    | Some left_at -> t.closed -. left_at < s.duration +. s.listed_max

let expire t ~overhead =
  let left = List.filter (fun s -> s.left_at <> None) t.segments in
  let left_after s =
    List.length (List.filter (fun s' -> s'.position > s.position) left)
  in
  let expired, kept =
    List.partition
      (fun s ->
        s.left_at <> None
        && (not (available t s))
        && Option.fold ~none:true ~some:(fun n -> left_after s >= n) overhead)
      t.segments
  in
  t.segments <- kept;
  expired

let lifetime t s =
  s.duration +. s.listed_max
  -. Option.fold ~none:0. ~some:(fun l -> t.closed -. l) s.left_at

let referenced_inits t = List.filter_map (fun s -> s.init) t.segments

(* Restored segments outside the window count as having left it at the
   restore, listed by a playlist as long as the restored window: retention can
   only be longer than in the process that saved them. *)
let restore ~next_position ~next_init_position ~discontinuity_count
    ~window_start segments =
  let t =
    {
      next_position;
      next_init_position;
      discontinuity_count;
      segments = [];
      window_start;
      closed = 0.;
    }
  in
  t.segments <-
    List.map
      (fun ((s : Persist.segment), size) ->
        {
          position = s.position;
          discontinuous = s.discontinuous;
          discontinuity = s.discontinuity;
          forced = s.forced;
          codecs = s.codecs;
          init = s.init;
          name = s.name;
          tags = s.tags;
          duration = s.duration;
          size;
          listed_max = 0.;
          left_at = (if s.position < window_start then Some 0. else None);
        })
      segments;
  let d = duration (listed t) in
  List.iter (fun s -> s.listed_max <- d) t.segments;
  t

let to_persisted t =
  List.map
    (fun s ->
      {
        Persist.position = s.position;
        discontinuous = s.discontinuous;
        discontinuity = s.discontinuity;
        forced = s.forced;
        codecs = s.codecs;
        init = s.init;
        name = s.name;
        tags = s.tags;
        duration = s.duration;
      })
    t.segments
