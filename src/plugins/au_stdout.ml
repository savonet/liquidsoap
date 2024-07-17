(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

(**
  * Read an input from stdout in AU format (vsound rulez...).
  *
  * @author Samuel Mimram, David Baelde
  *)

let int_of_word w =
  (int_of_char w.[0]) lsl 24 +
  (int_of_char w.[1]) lsl 16 +
  (int_of_char w.[2]) lsl 8 +
  int_of_char w.[3]

let get_four_bytes fd =
  let buf = String.create 4 in
    assert (input fd buf 0 4 = 4);
    buf

let get_int fd =
  int_of_word (get_four_bytes fd)

open Source

class au_stdout =
object
  inherit active_source

  method stype = Fallible

  val mutable stdout = None

  val mutable format = Mixer.Buffer.format
  val mutable buf = ""
  val mutable bs = 0

  method wake_up =
    let fd = Unix.open_process_in "vsound -s linphone" in
    let bformat = Mixer.Buffer.format in
      assert (get_four_bytes fd = ".snd");
      let () = get_int fd in
      let () = get_int fd in
      let () = get_int fd in
      let freq = get_int fd in
        format <- { Mixer.Buffer.format with
                      Mixer.sample_freq = freq ;
                      Mixer.channels = get_int fd } ;
        stdout <- Some fd ;
        let s = (((float format.Mixer.sample_freq)*.
                   (float format.Mixer.sample_size)*.
                   (float format.Mixer.channels)) /.
                  ((float bformat.Mixer.sample_freq)*.
                   (float bformat.Mixer.sample_size)*.
                   (float bformat.Mixer.channels))) in
          (* Check s is int *)
          bs <- int_of_float s ;
          buf <- String.create bs

  method sleep = ()
  (* TODO: close_process_in *)

  method is_ready = stdout <> None

  method abort_track = ()

  val abg = Mixer.Generator.create ()

  method get_frame ab =
    assert (Mixer.Buffer.position ab = 0) ;
    let fd = match stdout with None -> assert false | Some f -> f in
      really_input fd buf 0 bs ;
      Mixer.Generator.feed abg format buf ;
      Mixer.Buffer.fill ab abg ;
      assert (Mixer.Buffer.position ab = Mixer.Buffer.size)

end

let () =
  Lang.add_operator "au_stdout"
    ~descr:"Read the stdout of a program in AU format"
    []
    (fun p -> ((new au_stdout) :> source))
