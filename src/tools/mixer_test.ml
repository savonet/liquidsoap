(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

open Mixer

let random () =
  let ab = Mixer.Buffer.create () in
  let s = Mixer.Buffer.to_string ab in
    for i = 0 to Mixer.Buffer.size - 1 do
      s.[i] <- char_of_int (Random.int 256)
    done ;
    ab

let once () =
  let ab = random () in
  let abb = random () in
  let abs = random () in
    Mixer.Buffer.set_already ab Mixer.Buffer.size ;
    Mixer.Buffer.blankify abb ;
    ignore (Mixer.Buffer.sine abs 0 Mixer.Buffer.size 440 0.) ;

    Mixer.Buffer.change_volume ab 0 Mixer.Buffer.size 0.3 ;
    Mixer.Buffer.simple_filter abs 0 Mixer.Buffer.size 446 0.69 Mixer.Low_pass ;
    Mixer.Buffer.add abs 0 abb 0 Mixer.Buffer.size ;
    Mixer.Buffer.add ab 0 abs 0 Mixer.Buffer.size ;

    let b = Mixer.Generator.create () in
      Mixer.Generator.feed b
        { channels = 2 ;
          sample_freq = 22050 ;
          sample_size = 8 ;
          big_endian = true ;
          signed = false } (Mixer.Buffer.to_string ab) ;
      Mixer.Generator.feed b
        { channels = 1 ;
          sample_freq = 44100 ;
          sample_size = 32 ;
          big_endian = true ;
          signed = false } (Mixer.Buffer.to_string ab) ;
      Mixer.Buffer.free abb ;
      Mixer.Buffer.fill abb b

let _ =
  for i = 1 to 5 do
    once () ;
    Gc.full_major ()
  done ;
