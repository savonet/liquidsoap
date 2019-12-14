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

let file =
  try Sys.argv.(1)
  with _ ->
    Printf.printf "Usage: wav_test file.wav\n" ;
    exit (-1)

let fd = Wav.fopen file

let format = Wav.format fd

let abg = Mixer.Generator.create ()

let ab = Mixer.Buffer.create ()

let buflen = Mixer.Buffer.size

let buf = Bytes.make buflen 'x'

let running = ref true

let () =
  Printf.fprintf stderr "Info:\n%s\n" (Wav.info fd) ;
  print_string (Wav.header Mixer.Buffer.format) ;
  while !running do
    begin
      try
        while Mixer.Generator.should_be_feeded abg do
          let l = Wav.sample fd buf 0 buflen in
          Mixer.Generator.feed abg format (String.sub buf 0 l)
        done
      with End_of_file -> running := false
    end ;
    Mixer.Buffer.free ab ;
    Mixer.Buffer.fill ab abg ;
    print_string (Mixer.Buffer.to_string ab)
  done
