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
  type sample = float array

  type t = {
    channels : int;
    x : sample * sample;
    (* (x', x'') *)
    y : sample * sample;
    (* (y', y'') *)
    a1 : float;
    a2 : float;
    b0 : float;
    b1 : float;
    b2 : float;
  }

  (** Create and IIR filter. The coefficients are given for a samplerate of 48
      kHz and are adjusted for required target samplerate. *)
  let create ~channels ~samplerate ~a1 ~a2 ~b0 ~b1 ~b2 =
    let blank = Array.make channels 0. in
    let x = (blank, blank) in
    let y = (blank, blank) in
    if samplerate = 48000. then { channels; x; y; a1; a2; b0; b1; b2 }
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
      { channels; x; y; a1; a2; b0; b1; b2 })

  let stage1 =
    create ~a1:(-1.69065929318241) ~a2:0.73248077421585 ~b0:1.53512485958697
      ~b1:(-2.69169618940638) ~b2:1.19839281085285

  let stage2 =
    create ~a1:(-1.99004745483398) ~a2:0.99007225036621 ~b0:1. ~b1:(-2.) ~b2:1.

  external process : t -> float array -> float array -> unit
    = "liquidsoap_lufs_process"
  [@@noalloc]

  let process iir x =
    let channels = iir.channels in
    assert (Array.length x = channels);
    let y = Array.make channels 0. in
    process iir x y;
    y
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
    val mutable stage1 = id
    val mutable stage2 = id

    (** Current mean square (weighted sum over channels). *)
    val mutable ms = 0.

    (** Length of current mean square in samples *)
    val mutable ms_len = 0

    (** Last 100ms blocks. *)
    val mutable ms_blocks = []

    initializer
      self#on_wake_up (fun () ->
          let channels = self#audio_channels in
          let samplerate = self#samplerate in
          stage1 <- IIR.process (IIR.stage1 ~channels ~samplerate);
          stage2 <- IIR.process (IIR.stage2 ~channels ~samplerate))

    val mutable lufs_integrated = LufsIntegratedHistogram.create ()
    method lufs_integrated = LufsIntegratedHistogram.compute lufs_integrated

    method private add_integrated_block v =
      LufsIntegratedHistogram.append lufs_integrated v

    method private reset_lufs_integrated =
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

    method private process_frame ?(new_track = false) frame =
      if new_track then self#reset_lufs_integrated;
      let channels = self#audio_channels in
      let len_100ms = Frame.audio_of_seconds 0.1 in
      let position = AFrame.position frame in
      let buf = AFrame.pcm frame in
      for i = 0 to position - 1 do
        let x = Array.init channels (fun c -> buf.(c).(i)) in
        (* Prefilter. *)
        let x = stage1 x in
        let x = stage2 x in
        (* Add squares. *)
        for c = 0 to channels - 1 do
          let xc = x.(c) in
          ms <- ms +. (xc *. xc)
        done;
        ms_len <- ms_len + 1;
        (* When we have 100ms of squares we push the block. *)
        if ms_len >= len_100ms then (
          let v = ms /. float_of_int len_100ms in
          ms_blocks <- v :: ms_blocks;
          self#add_integrated_block v;
          ms <- 0.;
          ms_len <- 0)
      done;
      (* Keep only a limited (by the window) number of blocks. *)
      ms_blocks <- List.prefix (int_of_float (window () /. 0.1)) ms_blocks

    method private generate_frame =
      let frame = source#get_frame in
      (match self#split_frame frame with
        | _, Some frame when Frame.position frame > 0 ->
            self#process_frame ~new_track:true frame
        | frame, _ -> self#process_frame frame);
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
