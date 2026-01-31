(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
  log : Log.t;
  content_type : Frame_base.content_type;
  max_length : int option Atomic.t;
  content : Content.data Frame_base.Fields.t Atomic.t;
}

let make_content ?length content_type =
  Frame_base.add_timed_content
    (Frame_base.Fields.map (Content.make ?length) content_type)

let create ?(log = log) ?max_length ?length ?content content_type =
  {
    lock = Mutex.create ();
    max_length = Atomic.make max_length;
    log;
    content_type;
    content =
      Atomic.make
        (match content with
          | Some c -> c
          | None -> make_content ?length content_type);
  }

let get_field gen field = Frame_base.Fields.find field (Atomic.get gen.content)

let set_field =
  let add_field gen field content () =
    Atomic.set gen.content
      (Frame_base.Fields.add field content (Atomic.get gen.content))
  in
  fun gen field content ->
    Mutex_utils.mutexify gen.lock (add_field gen field content) ()

let max_length { max_length } = Atomic.get max_length
let content_type { content_type } = content_type
let set_max_length gen max_length = Atomic.set gen.max_length max_length

let field_length { content } field =
  Content.length (Frame_base.Fields.find field (Atomic.get content))

let media_content
    ?(pick =
      fun f _ ->
        f <> Frame_base.Fields.metadata && f <> Frame_base.Fields.track_marks)
    { content } =
  Frame_base.Fields.filter pick (Atomic.get content)

let length ?pick gen =
  Option.value ~default:0
    (Frame_base.Fields.fold
       (fun _ c -> function
         | None -> Some (Content.length c)
         | Some l' -> Some (min (Content.length c) l'))
       (media_content ?pick gen) None)

let _length = length

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

let _truncate ?(allow_desync = false) gen len =
  Atomic.set gen.content
    (Frame_base.Fields.map
       (fun content ->
         let len =
           if allow_desync then min len (Content.length content)
           else (
             assert (len <= Content.length content);
             len)
         in
         Content.truncate content len)
       (Atomic.get gen.content))

let truncate gen = Mutex_utils.mutexify gen.lock (_truncate gen)

let _keep gen len =
  Atomic.set gen.content
    (Frame_base.Fields.map
       (fun content ->
         assert (len <= Content.length content);
         Content.sub content 0 len)
       (Atomic.get gen.content))

let keep gen = Mutex_utils.mutexify gen.lock (_keep gen)

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

let slice gen = Mutex_utils.mutexify gen.lock (_slice gen)
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

let add_metadata ?pos gen =
  Mutex_utils.mutexify gen.lock (_add_metadata ?pos gen)

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
  Mutex_utils.mutexify gen.lock (fun () -> _add_track_mark ?pos gen) ()

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
    gen.log#severe
      "Generator max buffered length exceeded (%d < %d)! Dropping content.."
      max_length buffered_length;
    let excess = buffered_length - max_length in
    let dropped = min (length gen) excess in
    let dropped, allow_desync =
      if dropped = 0 then (
        gen.log#severe
          "Generator does not have synchronized content to drop. Content may \
           get out of sync!";
        (excess, true))
      else (dropped, false)
    in
    _truncate ~allow_desync gen dropped)

let put gen field =
  Mutex_utils.mutexify gen.lock (fun content -> _put gen field content)

let peek gen = Atomic.get gen.content
let peek_media gen = media_content gen

let append ?(offset = 0) ?length gen =
  Mutex_utils.mutexify gen.lock (fun frame ->
      let pos = _length gen in
      let length = Option.value ~default:(Frame_base.position frame) length in
      Atomic.set gen.content
        (Frame_base.Fields.mapi
           (fun field content ->
             match field with
               | _ when Frame_base.Fields.track_marks = field ->
                   List.iter
                     (fun p -> _add_track_mark ~pos:(pos + p) gen)
                     (Content_timed.Track_marks.get_data
                        (Content.sub
                           (Frame_base.Fields.find field frame)
                           offset length));
                   content
               | _ when Frame_base.Fields.metadata = field ->
                   List.iter
                     (fun (p, m) -> _add_metadata ~pos:(pos + p) gen m)
                     (Content_timed.Metadata.get_data
                        (Content.sub
                           (Frame_base.Fields.find field frame)
                           offset length));
                   content
               | _ ->
                   Content.append content
                     (Content.sub
                        (Frame_base.Fields.find field frame)
                        offset length))
           (Atomic.get gen.content)))
