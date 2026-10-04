type t = [ `Static | `Dynamic ] * Clock.Sync_source.t option

(* Two threads may compute this at once: the value is the same for both, so
   neither is made to wait. *)
let type_of_sources sources =
  let memo = Atomic.make None in
  fun () ->
    match Atomic.get memo with
      | Some sync_type -> sync_type
      | None ->
          let sync_type =
            if List.exists (fun s -> fst s#self_sync = `Dynamic) sources then
              `Dynamic
            else `Static
          in
          Atomic.set memo (Some sync_type);
          sync_type

let reporting sources =
  List.filter_map
    (fun s ->
      if s#is_ready then Option.map (fun sync -> (s, sync)) (snd s#self_sync)
      else None)
    sources

let rec first count = function
  | value :: rest when count > 0 -> value :: first (count - 1) rest
  | _ -> []

let conflict ~operator reporting =
  Clock.Sync_error
    {
      clock = operator;
      reported =
        List.map
          (fun (s, (sync : Clock.Sync_source.t)) ->
            {
              Clock.sync_source = sync.name;
              source = s#id;
              stack = first 3 s#stack;
            })
          reporting;
    }

let of_sources sources =
  let sync_type = type_of_sources sources in
  fun ?source () ->
    let reporting = reporting sources in
    let distinct =
      List.fold_left
        (fun distinct (_, sync) ->
          if List.exists (Clock.Sync_source.equal sync) distinct then distinct
          else sync :: distinct)
        [] reporting
    in
    match distinct with
      | [] -> (sync_type (), None)
      | [sync] -> (sync_type (), Some sync)
      | _ ->
          let operator = match source with Some s -> s#id | None -> "?" in
          raise (conflict ~operator reporting)

let same (a : Clock.Sync_source.t option) b = Clock.Sync_source.same a b
