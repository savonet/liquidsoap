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
  mutable max_length : int option;
  mutable content : Content.data Frame_base.Fields.t;
}

let add_timed_content content =
  Frame_base.Fields.add Frame_base.Fields.track_marks
    (Content.make Content.Track_marks.format)
    (Frame_base.Fields.add Frame_base.Fields.metadata
       (Content.make Content.Metadata.format)
       content)

let create ?(log = fun s -> log#info "%s" s) ?max_length ?length content_type =
  {
    lock = Mutex.create ();
    max_length;
    log;
    content_type;
    content =
      add_timed_content
        (Frame_base.Fields.map (Content.make ?length) content_type);
  }

let get_field gen =
  Tutils.mutexify gen.lock (fun field ->
      Frame_base.Fields.find field gen.content)

let set_field gen field =
  Tutils.mutexify gen.lock (fun content ->
      gen.content <- Frame_base.Fields.add field content gen.content)

let max_length gen = Tutils.mutexify gen.lock (fun () -> gen.max_length) ()
let content_type gen = Tutils.mutexify gen.lock (fun () -> gen.content_type) ()

let set_max_length gen =
  Tutils.mutexify gen.lock (fun max_length -> gen.max_length <- max_length)

let field_length { lock; content } =
  Tutils.mutexify lock (fun field ->
      Content.length (Frame_base.Fields.find field content))

let _media_content { content } =
  Frame_base.Fields.filter
    (fun f _ ->
      f <> Frame_base.Fields.metadata && f <> Frame_base.Fields.track_marks)
    content

let _length gen =
  Option.value ~default:0
    (Frame_base.Fields.fold
       (fun _ c -> function
         | None -> Some (Content.length c)
         | Some l' -> Some (min (Content.length c) l'))
       (_media_content gen) None)

let _buffered_length gen =
  Option.value ~default:0
    (Frame_base.Fields.fold
       (fun _ c -> function
         | None -> Some (Content.length c)
         | Some l' -> Some (max (Content.length c) l'))
       (_media_content gen) None)

let buffered_length gen =
  Tutils.mutexify gen.lock (fun () -> _buffered_length gen) ()

let length gen = Tutils.mutexify gen.lock (fun () -> _length gen) ()

let _remaining gen =
  match
    Content.Track_marks.get_data
      (Frame_base.Fields.find Frame_base.Fields.track_marks gen.content)
  with
    | p :: _ when p <= _length gen -> p
    | _ -> -1

let remaining gen = Tutils.mutexify gen.lock (fun () -> _remaining gen) ()

let _truncate gen len =
  gen.content <-
    Frame_base.Fields.map
      (fun content -> Content.truncate content len)
      gen.content

let truncate gen = Tutils.mutexify gen.lock (_truncate gen)

let clear gen =
  Tutils.mutexify gen.lock
    (fun () ->
      gen.content <-
        Frame_base.Fields.map
          (fun c -> Content.make ~length:0 (Content.format c))
          gen.content)
    ()

let _set_metadata gen =
  Content.Metadata.set_data
    (Frame_base.Fields.find Frame_base.Fields.metadata gen.content)

let _get_metadata gen =
  Content.Metadata.get_data
    (Frame_base.Fields.find Frame_base.Fields.metadata gen.content)

let _default_pos gen = function Some pos -> pos | None -> _length gen

let _add_metadata ?pos gen m =
  let pos = _default_pos gen pos in
  _set_metadata gen ((pos, m) :: _get_metadata gen)

let add_metadata ?pos gen = Tutils.mutexify gen.lock (_add_metadata ?pos gen)

let _set_track_marks gen =
  Content.Track_marks.set_data
    (Frame_base.Fields.find Frame_base.Fields.track_marks gen.content)

let _get_track_marks gen =
  Content.Track_marks.get_data
    (Frame_base.Fields.find Frame_base.Fields.track_marks gen.content)

let _add_track_mark ?pos gen =
  let pos = _default_pos gen pos in
  _set_track_marks gen (pos :: _get_track_marks gen)

let add_track_mark ?pos gen =
  Tutils.mutexify gen.lock (fun () -> _add_track_mark ?pos gen) ()

let _put gen field content =
  (match field with
    | f when f = Frame_base.Fields.track_marks ->
        List.iter
          (fun pos -> _add_track_mark ~pos gen)
          (Content.Track_marks.get_data content)
    | f when f = Frame_base.Fields.metadata ->
        List.iter
          (fun (pos, m) -> _add_metadata ~pos gen m)
          (Content.Metadata.get_data content)
    | _ ->
        gen.content <-
          Frame_base.Fields.add field
            (Content.append
               (Frame_base.Fields.find field gen.content)
               (Content.copy content))
            gen.content);

  let l = _buffered_length gen in
  match gen.max_length with
    | Some l' when l' < l ->
        let drop = l - l' in
        gen.log
          (Printf.sprintf
             "Generator max length exeeded (%d < %d)! Dropping %d ticks of \
              data.."
             l' l drop);
        _truncate gen drop
    | _ -> ()

let put gen field =
  Tutils.mutexify gen.lock (fun content -> _put gen field content)

let _get ?length gen =
  let length = Option.value ~default:(_length gen) length in
  if _length gen < length then
    failwith "Requested length is greater than buffer length!";
  let content =
    Frame_base.Fields.map (fun c -> Content.sub c 0 length) gen.content
  in
  gen.content <-
    Frame_base.Fields.map (fun c -> Content.truncate c length) gen.content;
  content

let get ?length gen = Tutils.mutexify gen.lock (fun () -> _get ?length gen) ()
let peek gen = Tutils.mutexify gen.lock (fun () -> gen.content) ()
let peek_media gen = Tutils.mutexify gen.lock (fun () -> _media_content gen) ()

(* The following is frame-specific and should hopefully go away when
   we switch to immutable content. *)

let _frame_position frame = match _remaining frame with -1 -> 0 | r -> r

let _frame_remaining frame =
  Lazy.force Frame_settings.size - _frame_position frame

let feed ?offset ?length ?fields gen =
  Tutils.mutexify gen.lock (fun frame ->
      Tutils.mutexify frame.lock
        (fun () ->
          let offset = Option.value ~default:0 offset in
          let length = Option.value ~default:(_frame_position frame) length in
          let fields =
            Option.value
              ~default:(List.map fst (Frame_base.Fields.bindings gen.content))
              fields
          in

          gen.content <-
            List.fold_left
              (fun content field ->
                match field with
                  (* The current use of generators is to explicitly add track marks via `Generator.add_track_mark`.
                     This will be changed when we switch to immutable content. *)
                  | f when f = Frame_base.Fields.track_marks -> content
                  | f when f = Frame_base.Fields.metadata ->
                      let gen_content =
                        Frame_base.Fields.find field gen.content
                      in
                      let frame_content =
                        Frame_base.Fields.find field frame.content
                      in
                      Content.Metadata.set_data gen_content
                        (Content.Metadata.get_data gen_content
                        @ Content.Metadata.get_data frame_content);
                      content
                  | _ ->
                      Frame_base.Fields.add field
                        (Content.append
                           (Frame_base.Fields.find field gen.content)
                           (Content.copy
                              (Content.sub
                                 (Frame_base.Fields.find field frame.content)
                                 offset length)))
                        content)
              gen.content fields)
        ())

let fill gen =
  Tutils.mutexify gen.lock (fun frame ->
      Tutils.mutexify frame.lock
        (fun () ->
          let available =
            match _remaining gen with -1 -> _length gen | l -> l
          in
          let len = min (_frame_remaining frame) available in
          let pos = _frame_position frame in
          gen.content <-
            Frame_base.Fields.mapi
              (fun field content ->
                let rem =
                  Content.sub content len (Content.length content - len)
                in
                (* TODO: make this append after after switch to immutable content. *)
                (match field with
                  (* Remove the first break in rem *)
                  | f when f = Frame_base.Fields.track_marks -> (
                      match Content.Track_marks.get_data rem with
                        | _ :: d -> Content.Track_marks.set_data rem d
                        | _ -> ())
                  | f when f = Frame_base.Fields.metadata ->
                      (* For metadata, it is expected that we only add new metadata and do not replace
                         exiting ones. *)
                      let frame_content =
                        Frame_base.Fields.find field frame.content
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
                      Content.blit content 0
                        (Frame_base.Fields.find f frame.content)
                        pos len);
                rem)
              gen.content;
          _add_track_mark ~pos:(pos + len) frame)
        ())
