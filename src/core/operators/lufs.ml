(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Source
open Extralib

module List = struct
  include List

  let mean l = List.fold_left ( +. ) 0. l /. float_of_int (List.length l)
end

(** Second order IIR filter. *)
module IIR = struct
  type stage
  type t = int * stage * stage

  external create :
    channels:int ->
    a1:float ->
    a2:float ->
    b0:float ->
    b1:float ->
    b2:float ->
    stage = "liquidsoap_lufs_create_bytecode" "liquidsoap_lufs_create_native"

  (** Create and IIR filter. The coefficients are given for a samplerate of 48
      kHz and are adjusted for required target samplerate. *)
  let create ~channels ~samplerate ~a1 ~a2 ~b0 ~b1 ~b2 =
    if samplerate = 48000. then create ~channels ~a1 ~a2 ~b0 ~b1 ~b2
    else (
      (* The coefficients of the specification are given for a 48 kHz samplerate,
         this computes the values for other samplerates. This is "strongly
         inspired" of https://github.com/klangfreund/LUFSMeter/ *)
      let k_q = (2. -. (2. *. a2)) /. (a2 -. a1 +. 1.) in
      let k = sqrt ((a1 +. a2 +. 1.) /. (a2 -. a1 +. 1.)) in
      let q = k /. k_q in
      let atan_k = atan k in
      let vb = (b0 -. b2) /. (1. -. a2) in
      let vh = (b0 -. b1 +. b2) /. (a2 -. a1 +. 1.) in
      let vl = (b0 +. b1 +. b2) /. (a1 +. a2 +. 1.) in
      let k = tan (atan_k *. 48000. /. samplerate) in
      let a = 1. /. (1. +. (k /. q) +. (k *. k)) in
      let b0 = (vh +. (vb *. k /. q) +. (vl *. k *. k)) *. a in
      let b1 = 2. *. ((vl *. k *. k) -. vh) *. a in
      let b2 = (vh -. (vb *. k /. q) +. (vl *. k *. k)) *. a in
      let a1 = 2. *. ((k *. k) -. 1.) *. a in
      let a2 = (1. -. (k /. q) +. (k *. k)) *. a in
      create ~channels ~a1 ~a2 ~b0 ~b1 ~b2)

  let create ~channels ~samplerate =
    let stage1 =
      create ~a1:(-1.69065929318241) ~a2:0.73248077421585 ~b0:1.53512485958697
        ~b1:(-2.69169618940638) ~b2:1.19839281085285 ~channels ~samplerate
    in
    let stage2 =
      create ~a1:(-1.99004745483398) ~a2:0.99007225036621 ~b0:1. ~b1:(-2.)
        ~b2:1. ~channels ~samplerate
    in
    (channels, stage1, stage2)

  external process : stage1:stage -> stage2:stage -> float array array -> float
    = "liquidsoap_lufs_process"

  let process (channels, stage1, stage2) samples =
    assert (Array.length samples = channels);
    process ~stage1 ~stage2 samples
end

(** Compute the loudness from the mean of squares. *)
let loudness z = -0.691 +. (10. *. log10 z)

let energy z = Float.pow 10. ((z +. 0.691) /. 10.)
let min_lufs = -70.

module LufsIntegratedHistogram = struct
  let granularity = 100.

  type histogram_entry = {
    mutable count : int;
    loudness : float;
    energy : float;
  }

  type histogram = {
    mutable pending : float list;
    mutable entries : (int * histogram_entry) list;
    mutable threshold : float;
    mutable threshold_count : int;
  }

  let pos loudness = int_of_float ((loudness -. min_lufs) *. granularity)

  let create () =
    { pending = []; entries = []; threshold = 0.; threshold_count = 0 }

  let get_entry h pos =
    try List.assoc pos h.entries
    with Not_found ->
      let loudness = (float pos /. granularity) +. min_lufs in
      let entry = { count = 0; loudness; energy = energy loudness } in
      h.entries <- (pos, entry) :: h.entries;
      entry

  let append h v =
    let blocks = v :: h.pending in
    if List.length blocks = 4 then (
      h.pending <- [];
      let power = List.mean blocks in
      let loudness = loudness power in
      if min_lufs <= loudness then (
        let entry = get_entry h (pos loudness) in
        entry.count <- entry.count + 1;
        h.threshold <- h.threshold +. power;
        h.threshold_count <- h.threshold_count + 1))
    else h.pending <- blocks

  let compute { entries; threshold; threshold_count } =
    let threshold = loudness (threshold /. float threshold_count) -. 10. in
    let power, total =
      List.fold_left
        (fun (power, total) (_, { count; loudness; energy }) ->
          if threshold <= loudness then
            (power +. (energy *. float count), total + count)
          else (power, total))
        (0., 0) entries
    in
    loudness (power /. float total)
end

class lufs window source =
  object (self)
    inherit operator [source] ~name:"lufs"
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method seek_source = source#seek_source
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    (** Last 100ms blocks. *)
    val mutable ms_blocks = []

    val mutable len_100ms = 0
    val mutable iir = None

    method private iir =
      match iir with
        | None ->
            let channels = self#audio_channels in
            let samplerate = self#samplerate in
            let h = IIR.create ~channels ~samplerate in
            iir <- Some h;
            h
        | Some v -> v

    initializer
      self#on_wake_up (fun () -> len_100ms <- Frame.main_of_seconds 0.1)

    val mutable lufs_integrated = LufsIntegratedHistogram.create ()
    method lufs_integrated = LufsIntegratedHistogram.compute lufs_integrated

    method private add_integrated_block v =
      LufsIntegratedHistogram.append lufs_integrated v

    method private reset_lufs_integrated =
      iir <- None;
      lufs_integrated <- LufsIntegratedHistogram.create ()

    (** Compute LUFS. *)
    method lufs =
      (* Compute ms of overlapping 400ms blocks. *)
      let blocks =
        let rec aux b' b'' b''' = function
          | b :: l -> ((b +. b' +. b'' +. b''') /. 4.) :: aux b b' b'' l
          | [] -> []
        in
        let aux = function b :: b' :: b'' :: l -> aux b b' b'' l | _ -> [] in
        aux ms_blocks
      in
      (* Blocks over absolute threshold. *)
      let absolute = List.filter (fun z -> loudness z > min_lufs) blocks in
      (* Relative threshold. *)
      let threshold = loudness (List.mean absolute) -. 10. in
      (* Blocks over relative threshold. *)
      let relative = List.filter (fun z -> loudness z > threshold) blocks in
      loudness (List.mean relative)

    (** Momentary LUFS. *)
    method lufs_momentary = loudness (List.mean (List.prefix 4 ms_blocks))

    method private process_frame frame =
      Generator.append self#buffer frame;
      while len_100ms < Generator.length self#buffer do
        let frame = Generator.slice self#buffer len_100ms in
        if Frame.has_track_marks frame then self#reset_lufs_integrated;
        let buf = AFrame.pcm frame in
        let power = IIR.process self#iir buf in
        self#add_integrated_block power;
        ms_blocks <- power :: ms_blocks
      done;
      (* Keep only a limited (by the window) number of blocks. *)
      ms_blocks <- List.prefix (int_of_float (window () /. 0.1)) ms_blocks

    method private generate_frame =
      let frame = source#get_frame in
      self#process_frame frame;
      frame
  end

let _ =
  let return_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "lufs" ~category:`Visualization
    ~meth:
      [
        ( "lufs",
          ([], Lang.fun_t [] Lang.float_t),
          "Current value for the LUFS (short-term value computed over the \
           duration specified by the `window` parameter).",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#lufs) );
        ( "lufs_integrated",
          ([], Lang.fun_t [] Lang.float_t),
          "Average LUFS value over the current track.",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#lufs_integrated) );
        ( "lufs_momentary",
          ([], Lang.fun_t [] Lang.float_t),
          "Momentary LUFS (over a 400ms window).",
          fun s -> Lang.val_fun [] (fun _ -> Lang.float s#lufs_momentary) );
      ]
    ~return_t
    ~descr:
      "Compute current LUFS of the source according to the EBU R128 standard. \
       It returns the source with a method to compute the current value."
    [
      ( "window",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 3.),
        Some "Duration of the window (in seconds) used to compute the LUFS." );
      ("", Lang.source_t return_t, None, None);
    ]
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let window = Lang.to_float_getter (f "window") in
      new lufs window src)
