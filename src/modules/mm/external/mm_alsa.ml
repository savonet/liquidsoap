(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

let rw channels samplerate ?(device = "default") ?(playback = false)
    ?(capture = false) ?(blocking = true) ?(buffer_size = 1024) ?(periods = 4)
    () =
  object
    (* inherit Audio.IO.rw_bufferized *)
    method version = Alsa.get_version ()

    val dev =
      Alsa.Pcm.open_pcm device
        ((if playback then [Alsa.Pcm.Playback] else [])
        @ if capture then [Alsa.Pcm.Capture] else [])
        []

    method delay = Alsa.Pcm.get_delay dev
    method prepare = Alsa.Pcm.prepare dev
    method wait t = Alsa.Pcm.wait dev t
    method recover e = Alsa.Pcm.recover dev e
    val mutable buffer_size = buffer_size

    initializer
      let params = Alsa.Pcm.get_params dev in
      Alsa.Pcm.set_access dev params Alsa.Pcm.Access_rw_noninterleaved;
      Alsa.Pcm.set_format dev params Alsa.Pcm.Format_float;
      Alsa.Pcm.set_channels dev params channels;
      Alsa.Pcm.set_periods dev params periods Alsa.Dir_eq;
      assert (
        Alsa.Pcm.set_rate_near dev params samplerate Alsa.Dir_eq = samplerate);
      buffer_size <- Alsa.Pcm.set_buffer_size_near dev params buffer_size;
      Alsa.Pcm.set_params dev params;
      Alsa.Pcm.set_nonblock dev (not blocking)

    method read = Alsa.Pcm.readn_float dev
    method write = Alsa.Pcm.writen_float dev
    method close = Alsa.Pcm.close dev
  end
