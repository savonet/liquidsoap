let id x = x

module List = struct
  include List

  let init n f =
    let rec aux k = if k = n then [] else f k :: aux (k + 1) in
    aux 0

  let rec may_map f = function
    | x :: t -> (
        match f x with Some x -> x :: may_map f t | None -> may_map f t )
    | [] -> []

  let rec assoc_nth l n = function
    | [] -> raise Not_found
    | (x, v) :: t when x = l -> if n = 0 then v else assoc_nth l (n - 1) t
    | _ :: t -> assoc_nth l n t

  let assoc_all x l = may_map (fun (y, v) -> if x = y then Some v else None) l
  let rec last = function [x] -> x | _ :: l -> last l | [] -> raise Not_found
end

module String = struct
  include String

  let split_char c s =
    let rec aux res n =
      try
        let n' = index_from s n c in
        let s0 = sub s n (n' - n) in
        aux (s0 :: res) (n' + 1)
      with Not_found -> (if n = 0 then s else sub s n (length s - n)) :: res
    in
    List.rev (aux [] 0)
end

let read_retry read buf off len =
  let r = ref 0 in
  let loop = ref true in
  while !loop do
    let n = read buf (off + !r) (len - !r) in
    r := !r + n;
    loop := !r <> 0 && !r < len && n <> 0
  done;
  !r

module Unix = struct
  include Unix

  let read_retry fd = read_retry (read fd)

  let rm_dir dir =
    let rec finddepth f roots =
      Array.iter
        (fun root ->
          ( match lstat root with
            | { st_kind = S_DIR } ->
                finddepth f
                  (Array.map (Filename.concat root) (Sys.readdir root))
            | _ -> () );
          f root)
        roots
    in
    let zap path =
      match lstat path with
        | { st_kind = S_DIR } -> rmdir path
        | _ -> unlink path
    in
    finddepth zap [| dir |];
    rmdir dir
end

module Filename = struct
  include Filename

  let rand_digits () =
    let rand = Random.State.(bits (make_self_init ()) land 0xFFFFFF) in
    Printf.sprintf "%06x" rand

  let mk_temp_dir ?(mode = 0o700) ?dir prefix suffix =
    let dir = match dir with Some d -> d | None -> get_temp_dir_name () in
    let raise_err msg = raise (Sys_error msg) in
    let rec loop count =
      if count < 0 then raise_err "mk_temp_dir: too many failing attemps"
      else (
        let dir =
          Printf.sprintf "%s/%s%s%s" dir prefix (rand_digits ()) suffix
        in
        try
          Unix.mkdir dir mode;
          dir
        with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
          | Unix.Unix_error (e, _, _) ->
              raise_err ("mk_temp_dir: " ^ Unix.error_message e) )
    in
    loop 1000
end
