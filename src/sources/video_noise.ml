(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

class noise ~kind duration =
  let nb_frames =
    if duration <= 0. then None else Some (Frame.video_of_seconds duration)
  in
object
  inherit Source.source kind

  method stype = Source.Infallible
  method is_ready = true

  val mutable remaining = nb_frames
  method remaining =
    match remaining with
      | None -> -1
      | Some remaining -> Frame.master_of_video remaining

  val mutable must_fail = false
  method abort_track = must_fail <- true

  method private get_frame ab =
    if must_fail then begin
      VFrame.add_break ab (VFrame.position ab);
      remaining <- nb_frames;
      must_fail <- false
    end else
      let off = VFrame.position ab in
      let b = VFrame.content_of_type ~channels:1 ab off in
      let size =
        match remaining with
          | None -> VFrame.size ab - off
          | Some r ->
              let size = min (VFrame.size ab - off) r in
                remaining <- Some (r - size) ;
                size
      in
        for c = 0 to Array.length b - 1 do
          let buf_c = b.(c) in
            for i = 0 to size - 1 do
              (* TODO directly randomize the stream? *)
              let frame =
                RGB.create
                  (Lazy.force Frame.video_width)
                  (Lazy.force Frame.video_height)
              in
                RGB.randomize frame;
                buf_c.(off+i) <- frame
            done;
        done;
        VFrame.add_break ab (off+size) ;
        if VFrame.is_partial ab then begin
          assert (remaining = Some 0) ;
          remaining <- nb_frames
        end

end

let () =
  Lang.add_operator "video.noise"
    ~category:Lang.Input
    ~descr:"Generate white noise."
    [ "duration", Lang.float_t, Some (Lang.float 0.),
      Some "Duration of noise tracks. \
            Zero means no tracks, just a continuous stream." ]
    ~kind:Lang.video_only
    (fun p kind ->
       new noise ~kind (Lang.to_float (List.assoc "duration" p)))
