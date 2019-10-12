(*****************************************************************************

   Liquidsoap, a programmable audio stream generator.
   Copyright 2003-2019 Savonet team

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

(** Output using SDL lib. *)


class output ~infallible ~on_start ~on_stop ~autostart ~kind source =
  let video_width    = Lazy.force Frame.video_width in
  let video_height   = Lazy.force Frame.video_height in
  let () = Sdl_utils.init [`VIDEO] in
  object (self)
    inherit Output.output ~name:"sdl" ~output_kind:"output.sdl"
        ~infallible ~on_start ~on_stop
        ~content_kind:kind source autostart

    val mutable fullscreen = false

    method output_start =
      Sdlevent.enable_events (Sdlevent.quit_mask lor Sdlevent.keydown_mask);
      (* Try to get 32bpp because it's faster (twice as fast here),
       * but accept other formats too. *)
      ignore (Sdlvideo.set_video_mode
                ~w:video_width ~h:video_height
                ~bpp:32 [`ANYFORMAT;`DOUBLEBUF]) ;
      self#log#info "Initialized SDL video surface with %dbpp."
        (Sdlvideo.surface_bpp (Sdlvideo.get_video_surface ()))

    (** We don't care about latency. *)
    method output_reset = ()

    (** Stop SDL. We have to assume that there's only one SDL output anyway. *)
    method output_stop = Sdl.quit ()

    method process_events =
      match Sdlevent.poll () with
        | Some Sdlevent.QUIT ->
          (* Avoid an immediate restart (which would happen with autostart).
           * But do not cancel autostart.
           * We should perhaps have a method in the output class for that
           * kind of thing, and try to get an uniform behavior. *)
          request_start <- false;
          request_stop <- true
        | Some (Sdlevent.KEYDOWN k) ->
          (
            match k.Sdlevent.keysym with
              | Sdlkey.KEY_f ->
                fullscreen <- not fullscreen;
                let mode = [`ANYFORMAT;`DOUBLEBUF] in
                let mode = if fullscreen then `FULLSCREEN::mode else mode in
                ignore (Sdlvideo.set_video_mode
                          ~w:video_width ~h:video_height
                          ~bpp:32 mode);
              | Sdlkey.KEY_q ->
                Sdlevent.add [Sdlevent.QUIT]
              | _ -> ()
          );
          self#process_events
        | Some _ ->
          self#process_events
        | None -> ()

    method output_send buf =
      self#process_events;
      let surface = Sdlvideo.get_video_surface () in
      (* We only display the first image of each frame *)
      let rgb =
        let stop,c = Frame.content buf 0 in
        assert (stop = Lazy.force Frame.size) ;
        (Video.get c.Frame.video.(0) 0)
      in
      begin match Sdlvideo.surface_bpp surface with
        | 16 -> Sdl_utils.to_16 rgb surface
        | 32 -> Sdl_utils.to_32 rgb surface
        | i -> failwith (Printf.sprintf "Unsupported format %dbpp" i)
      end ;
      Sdlvideo.flip surface

  end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 Lang.video_only in
  Lang.add_operator "output.sdl" ~active:true
    (Output.proto @ [
        "", Lang.source_t k, None, None
      ])
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.Output
    ~descr:"Display a video using SDL."
    (fun p kind ->
       let autostart = Lang.to_bool (List.assoc "start" p) in
       let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
       let on_start =
         let f = List.assoc "on_start" p in
         fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
       in
       let on_stop =
         let f = List.assoc "on_stop" p in
         fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
       in
       let source = List.assoc "" p in
       ((new output ~infallible ~autostart ~on_start ~on_stop
          ~kind source):>Source.source))
