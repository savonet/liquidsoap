(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

let log = Log.make ["generator"]

type t = {
  lock : Mutex.t;
  log : string -> unit;
  content_type : Frame_base.content_type;
  max_length : int option Atomic.t;
  content : Content.data Frame_base.Fields.t Atomic.t;
}

let make_content ?length content_type =
  Frame_base.add_timed_content
    (Frame_base.Fields.map (Content.make ?length) content_type)

let create ?(log = fun s -> log#info "%s" s) ?max_length ?length content_type =
  {
    lock = Mutex.create ();
    max_length = Atomic.make max_length;
    log;
    content_type;
    content = Atomic.make (make_content ?length content_type);
  }

let get_field gen field = Frame_base.Fields.find field (Atomic.get gen.content)

let set_field =
  let add_field gen field content () =
    Atomic.set gen.content
      (Frame_base.Fields.add field content (Atomic.get gen.content))
  in
  fun gen field content ->
    Tutils.mutexify gen.lock (add_field gen field content) ()

let max_length { max_length } = Atomic.get max_length
let content_type { content_type } = content_type
let set_max_length gen max_length = Atomic.set gen.max_length max_length

let field_length { content } field =
  Content.length (Frame_base.Fields.find field (Atomic.get content))

let media_content { content } =
  Frame_base.Fields.filter
    (fun f _ ->
      f <> Frame_base.Fields.metadata && f <> Frame_base.Fields.track_marks)
    (Atomic.get content)

let length gen =
  Option.value ~default:0
    (Frame_base.Fields.fold
       (fun _ c -> function
         | None -> Some (Content.length c)
         | Some l' -> Some (min (Content.length c) l'))
       (media_content gen) None)

let buffered_length gen =
  Option.value ~default:0
    (Frame_base.Fields.fold
       (fun _ c -> function
         | None -> Some (Content.length c)
         | Some l' -> Some (max (Content.length c) l'))
       (media_content gen) None)

let remaining gen =
  match
    Content.Track_marks.get_data
      (Frame_base.Fields.find Frame_base.Fields.track_marks
         (Atomic.get gen.content))
  with
    | p :: _ when p <= length gen -> p
    | _ -> -1

let _truncate gen len =
  Atomic.set gen.content
    (Frame_base.Fields.map
       (fun content ->
         if len <= Content.length content then Content.truncate content len
         else content)
       (Atomic.get gen.content))

let truncate gen = Tutils.mutexify gen.lock (_truncate gen)

let _slice gen len =
  let content = Atomic.get gen.content in
  let len =
    Frame_base.Fields.fold
      (fun _ c len -> min (Content.length c) len)
      content len
  in
  let slice = Frame_base.Fields.map (fun c -> Content.sub c 0 len) content in
  _truncate gen len;
  slice

let slice gen = Tutils.mutexify gen.lock (_slice gen)
let clear gen = Atomic.set gen.content (make_content ~length:0 gen.content_type)

let _set_metadata gen =
  Content.Metadata.set_data
    (Frame_base.Fields.find Frame_base.Fields.metadata (Atomic.get gen.content))

let get_metadata gen =
  Content.Metadata.get_data
    (Frame_base.Fields.find Frame_base.Fields.metadata (Atomic.get gen.content))

let default_pos gen = function Some pos -> pos | None -> length gen

let _add_metadata ?pos gen m =
  let pos = default_pos gen pos in
  _set_metadata gen ((pos, m) :: get_metadata gen)

let add_metadata ?pos gen = Tutils.mutexify gen.lock (_add_metadata ?pos gen)

let _set_track_marks gen =
  Content.Track_marks.set_data
    (Frame_base.Fields.find Frame_base.Fields.track_marks
       (Atomic.get gen.content))

let get_track_marks gen =
  Content.Track_marks.get_data
    (Frame_base.Fields.find Frame_base.Fields.track_marks
       (Atomic.get gen.content))

let _add_track_mark ?pos gen =
  let pos = default_pos gen pos in
  _set_track_marks gen (pos :: get_track_marks gen)

let add_track_mark ?pos gen =
  Tutils.mutexify gen.lock (fun () -> _add_track_mark ?pos gen) ()

let _put gen field new_content =
  (match field with
    | f when f = Frame_base.Fields.track_marks ->
        List.iter
          (fun pos -> _add_track_mark ~pos gen)
          (Content.Track_marks.get_data new_content)
    | f when f = Frame_base.Fields.metadata ->
        List.iter
          (fun (pos, m) -> _add_metadata ~pos gen m)
          (Content.Metadata.get_data new_content)
    | _ ->
        let current_content = get_field gen field in
        let content = Content.append current_content new_content in
        Atomic.set gen.content
          (Frame_base.Fields.add field content (Atomic.get gen.content)));
  let max_length = Option.value ~default:(-1) (Atomic.get gen.max_length) in
  let buffered_length = buffered_length gen in
  if 0 <= max_length && max_length < buffered_length then (
    gen.log
      (Printf.sprintf
         "Generator max length exeeded (%d < %d)! Dropping content.." max_length
         buffered_length);
    _truncate gen (buffered_length - max_length))

let put gen field =
  Tutils.mutexify gen.lock (fun content -> _put gen field content)

let _get =
  let len = length in
  fun ?length gen ->
    let length = Option.value ~default:(len gen) length in
    if len gen < length then
      failwith "Requested length is greater than buffer length!";
    let content =
      Frame_base.Fields.map
        (fun c -> Content.sub c 0 length)
        (Atomic.get gen.content)
    in
    Atomic.set gen.content
      (Frame_base.Fields.map
         (fun c -> Content.truncate c length)
         (Atomic.get gen.content));
    content

let get ?length gen = Tutils.mutexify gen.lock (fun () -> _get ?length gen) ()
let peek gen = Atomic.get gen.content
let peek_media gen = media_content gen

(* The following is frame-specific and should hopefully go away when
   we switch to immutable content. *)

let frame_position frame =
  match
    List.rev
      (Content.Track_marks.get_data
         (Frame_base.Fields.find Frame_base.Fields.track_marks
            (Atomic.get frame.content)))
  with
    | p :: _ -> p
    | _ -> 0

let frame_remaining frame =
  Lazy.force Frame_settings.size - frame_position frame

let feed ?offset ?length ?fields gen =
  Tutils.mutexify gen.lock (fun frame ->
      Tutils.mutexify frame.lock
        (fun () ->
          let offset = Option.value ~default:0 offset in
          let length = Option.value ~default:(frame_position frame) length in

          let gen_content = Atomic.get gen.content in
          let frame_content = Atomic.get frame.content in

          let fields =
            Option.value
              ~default:(List.map fst (Frame_base.Fields.bindings gen_content))
              fields
          in

          Atomic.set gen.content
            (List.fold_left
               (fun content field ->
                 match field with
                   (* The current use of generators is to explicitly add track marks via `Generator.add_track_mark`.
                      This will be changed when we switch to immutable content. *)
                   | f when f = Frame_base.Fields.track_marks -> content
                   | f when f = Frame_base.Fields.metadata ->
                       let gen_content =
                         Frame_base.Fields.find field gen_content
                       in
                       let frame_content =
                         Frame_base.Fields.find field frame_content
                       in
                       Content.Metadata.set_data gen_content
                         (Content.Metadata.get_data gen_content
                         @ Content.Metadata.get_data frame_content);
                       content
                   | _ ->
                       Frame_base.Fields.add field
                         (Content.append
                            (Frame_base.Fields.find field gen_content)
                            (Content.copy
                               (Content.sub
                                  (Frame_base.Fields.find field frame_content)
                                  offset length)))
                         content)
               gen_content fields))
        ())

let fill gen =
  Tutils.mutexify gen.lock (fun frame ->
      Tutils.mutexify frame.lock
        (fun () ->
          let available =
            match remaining gen with -1 -> length gen | l -> l
          in
          let len = min (frame_remaining frame) available in
          let pos = frame_position frame in
          let new_pos = pos + len in

          let gen_content = Atomic.get gen.content in
          let frame_content = Atomic.get frame.content in

          Atomic.set gen.content
            (Frame_base.Fields.mapi
               (fun field content ->
                 let rem =
                   Content.sub content len (Content.length content - len)
                 in
                 (* TODO: make this append after after switch to immutable content. *)
                 (match field with
                   (* Remove the track mark if it is was used to compute remaining and it's not
                      at frame boundaries. *)
                   | f when f = Frame_base.Fields.track_marks -> (
                       match Content.Track_marks.get_data rem with
                         | 0 :: d when new_pos <> Lazy.force Frame_settings.size
                           ->
                             Content.Track_marks.set_data rem d
                         | _ -> ())
                   | f when f = Frame_base.Fields.metadata ->
                       (* For metadata, it is expected that we only add new metadata and do not replace
                          exiting ones. *)
                       let frame_content =
                         Frame_base.Fields.find field frame_content
                       in
                       let content =
                         Content.Metadata.get_data (Content.sub content 0 len)
                       in
                       let content =
                         List.map (fun (p, m) -> (pos + p, m)) content
                       in
                       Content.Metadata.set_data frame_content
                         (Content.Metadata.get_data frame_content @ content)
                   | f ->
                       Content.fill content 0
                         (Frame_base.Fields.find f frame_content)
                         pos len);
                 rem)
               gen_content);
          _add_track_mark ~pos:new_pos frame)
        ())
