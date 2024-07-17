(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

open Source

let average_diff delta buf ofs len =
  let s = ref 0. in
    for i = 0 to len - delta - 1 do
      for c = 0 to Array.length buf - 1 do
        s := !s +. abs_float (buf.(c).(ofs + i + delta) -. buf.(c).(ofs + i))
      done
    done;
    !s /. (float ((len - delta) * (Array.length buf)))

let note_of_freq f =
  let x = log (f /. 440.) /. log 2. +. 1. in
  let x = if x < 0. then 100. +. x else x in
  let x, _ = modf x in
    int_of_float (x *. 12.) mod 12

let string_of_note = function
  | 0 -> "A"
  | 1 -> "A#/Bb"
  | 2 -> "B"
  | 3 -> "C"
  | 4 -> "C#/Db"
  | 5 -> "D"
  | 6 -> "D#/Eb"
  | 7 -> "E"
  | 8 -> "F"
  | 9 -> "F#/Gb"
  | 10 -> "G"
  | 11 -> "G#/Ab"
  | _ -> assert false

class pitch every length freq_min freq_max (source:source) =
  (** Compute a wave length from a frequency. *)
  let samples_per_second = float (Fmt.samples_per_second ()) in
  let channels = Fmt.channels () in
  let wl f = int_of_float (samples_per_second /. f) in
  let length = Fmt.samples_of_seconds length in
object (self)
  inherit operator [source] as super

  val ring = Ringbuffer.create channels (2 * length)

  (** Array used to get data to analyze. *)
  val databuf = Array.init channels (fun _ -> Array.make length 0.)

  val mutable computations = -1

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method get_frame buf =
    source#get buf;
    let buf = AFrame.get_float_pcm buf in
      Ringbuffer.write ring buf 0 (Array.length buf.(0));
      if Ringbuffer.read_space ring > length then
        Ringbuffer.read_advance ring (Ringbuffer.read_space ring - length);
      computations <- (computations + 1) mod every;
      if computations = 0 && Ringbuffer.read_space ring >= length then
        let wl_min = wl freq_max in
        let wl_max = wl freq_min in
        let d_opt = ref infinity in
        let wl_opt = ref 0 in
          Ringbuffer.read ring databuf 0 length;
          for l = wl_min to wl_max do
            let d = average_diff l databuf 0 length in
              if d < !d_opt then
                (
                  (* Printf.printf "d: %.02f      l: %d\n%!" d l; *)
                  d_opt := d;
                  wl_opt := l
                )
          done
          (*let f = samples_per_second /. float !wl_opt in
          let f = if f > freq_max then 0. else f in
            Printf.printf "Found frequency: %.02f (%s)\n%!" f (string_of_note (note_of_freq f))*)
end

let () =
  Lang.add_operator "pitch"
    [
      "length", Lang.float_t, Some (Lang.float 0.1), Some "Length in seconds of the analysis window";
      "freq_min", Lang.float_t, Some (Lang.float 40.), Some "Minimal frequency";
      "freq_max", Lang.float_t, Some (Lang.float 10000.), Some "Maximal frequency";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.SoundProcessing
    ~descr:"Compute the pitch of a sound"
    ~flags:[Lang.Hidden; Lang.Experimental]
    (fun p ->
       let f v = List.assoc v p in
       let length = Lang.to_float (f "length") in
       let freq_min = Lang.to_float (f "freq_min") in
       let freq_max = Lang.to_float (f "freq_max") in
       let src = Lang.to_source (f "") in
         ((new pitch 10 length freq_min freq_max src):>source)
    )
