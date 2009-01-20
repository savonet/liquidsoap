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

let conf_raw_buffering =
  Dtools.Conf.string ~p:(Root.conf#plug "buffering_kind") ~d:"default" "Kind of buffering for audio data (default|raw|disk|disk_manyfiles|disk_noring)."
    ~comments:[
      "If set to raw, liquidsoap will use raw s16le pcm format when buffering audio data.";
      "If set to disk, liquidsoap will store buffered data on disk (disk_manyfiles is the same but is a bit faster at the expense of creating many files).";
      "Both non-default options can save a lot of memory when buffering a lot of data, ";
      "at the cost of some computational power.";
    ]

let create_buffer chans len =
  Array.init chans (fun _ -> Array.make len 0.)

let sub buf ofs len =
  if ofs = 0 && len = Array.length buf then
    buf
  else
    Array.map (fun a -> Array.sub a ofs len) buf

external caml_float_pcm_convert_le_byte : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_le_byte" "caml_float_pcm_convert_le_native"

external caml_float_pcm_convert_be_byte : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_be_byte" "caml_float_pcm_convert_be_native"

let resample_s16le
      src src_off len signed samplesize big_endian
      ratio dst dst_off =
  if big_endian then
    caml_float_pcm_convert_be_byte
      src src_off len signed samplesize big_endian
      ratio dst dst_off
  else
    caml_float_pcm_convert_le_byte
      src src_off len signed samplesize big_endian
      ratio dst dst_off

external to_s16le : float array array -> int -> int -> string -> int -> int
         = "caml_float_pcm_to_s16le"

let to_s16le_ni buf ofs len dst dst_ofs =
  let ans = ref 0 in
    for c = 0 to Array.length buf - 1 do
      ans := to_s16le [|buf.(c)|] ofs len dst.(c) dst_ofs;
    done;
    !ans

external from_s16le : float array array -> int -> string -> int -> int -> unit
         = "caml_float_pcm_from_s16le"

let from_s16le_ni dbuf dofs buf ofs len =
  for c = 0 to Array.length buf - 1 do
    from_s16le [|dbuf.(c)|] dofs buf.(c) ofs len
  done

external float_blit : float array -> int -> float array -> int -> int -> unit
     = "caml_float_array_blit"

let native_resample ratio inbuf offs len =
  if ratio = 1. then
        let outbuf = Array.make len 0. in
          float_blit inbuf offs outbuf 0 len;
          outbuf
  else
    let outlen = int_of_float (float len *. ratio) in
    let outbuf = Array.make outlen 0. in
      for i = 0 to outlen - 1 do
        let inidx = min (int_of_float (float i /. ratio)) (len - 1) in
          outbuf.(i) <- inbuf.(inidx + offs)
      done;
      outbuf

let get_float_pcm b =
  let tracks = Array.to_list (Frame.get_tracks b) in
  let ans =
    List.fold_left
      (fun l t ->
         match t with
           | Frame.Float_pcm (_,a) -> a::l
           | _ -> l
      ) [] tracks in
    Array.of_list ans

module Raw_queue =
struct

  type t = (int*string) Queue.t

  let create = Queue.create

  let add buf q =
    let chans = Array.length buf in
    let len = Array.length buf.(0) in
    let slen = 2 * chans * len in
    let sbuf = String.create slen in
    ignore(to_s16le buf 0 len sbuf 0);
    Queue.add (chans,sbuf) q

  let from_s16le (chans,sbuf) =
    let slen = String.length sbuf in
    let len = slen / (chans * 2) in
    let buf = Array.make chans (Array.make len 0.) in
    ignore(from_s16le buf 0 sbuf 0 len);
    buf

  let peek q = from_s16le (Queue.peek q)

  let take q = from_s16le (Queue.take q)
end

module Disk_queue =
struct
  (** Resizable float arrays. *)
  module B =
  struct
    (* Option type is for handling empty arrays... *)
    type t = int * (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t option ref

    let create fd = fd, ref None

    let resize (fd, ba) len =
      if len = 0 then
        ba := None
      else
        ba := Some (Bigarray.Array1.map_file fd Bigarray.float32 Bigarray.c_layout true len)

    let length (fd, ba) =
      match !ba with
        | None -> 0
        | Some ba -> Bigarray.Array1.dim ba

    let append ((fd, ba) as b) buf =
      let buflen = Array.length buf in
        if buflen <> 0 then
          let oldlen = length b in
            resize b (oldlen + buflen);
            let ba = Utils.get_some !ba in
              for i = 0 to buflen - 1 do
                Bigarray.Array1.set ba (oldlen + i) buf.(i)
              done

    let peek (fd, ba) len =
      if len = 0 then
        [||]
      else
        let ba = Utils.get_some !ba in
          Array.init len (fun i -> Bigarray.Array1.get ba i)

    let take ((fd, ba) as b) len =
      match !ba with
        | None ->
            assert (len = 0);
            [||]
        | Some ba ->
            let balen = length b in
            let ans = peek b len in
              (* TODO: more efficient blitting? Official way is much less
               * efficient... *)
              (* Bigarray.Array1.blit (Bigarray.Array1.sub ba len (balen - len)) (Bigarray.Array1.sub ba 0 (balen - len)); *)
              for i = len to balen - 1 do
                Bigarray.Array1.set ba (i - len) (Bigarray.Array1.get ba i)
              done;
              resize b (balen - len);
              ans
  end

  type t = (int Queue.t * (string * B.t) array)

  let add buf (q, a) =
    let chans = Array.length buf in
      Queue.add (Array.length buf.(0)) q;
      for i = 0 to chans - 1 do
        B.append a.(i) buf.(i)
      done

  let peek (q, a) =
    let chans = Array.length a in
    let len = Queue.peek q in
      Array.init chans (fun c -> B.peek a.(c) len)

  let take (q, a) =
    let chans = Array.length a in
    let len = Queue.peek q in
      Array.init chans (fun c -> B.take a.(c) len)

  let create () =
    let chans = Fmt.channels () in
      Queue.create (),
      Array.init
        chans
        (fun c ->
           let fname = Filename.temp_file "liquidsoap_buffer" "" in
           let fd = Unix.openfile fname [Unix.O_RDWR] 0o600 in
             ignore (Dtools.Init.at_stop (fun () -> Unix.unlink fname));
             B.create fd
        )
end

module Disk_ringbuffer_queue =
struct
  (** Resizable float ringbuffers. *)
  module B =
  struct
    (* Option type is for handling empty arrays... *)
    type t = {
      mutable size : int ;
      fd : Unix.file_descr;
      mutable buffer : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t option;
      mutable rpos : int ;
      mutable wpos : int
    }

    let create fd =
      {
        size = 0;
        fd = fd;
        buffer = None;
        rpos = 0;
        wpos = 0;
      }

    let read_space t =
      if t.wpos >= t.rpos then (t.wpos - t.rpos)
      else t.size - (t.rpos - t.wpos)

    let write_space t =
      if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
      else (t.rpos - t.wpos) - 1

    let read_advance t n =
      assert (n <= read_space t);
      if t.rpos + n < t.size then t.rpos <- t.rpos + n
      else t.rpos <- t.rpos + n - t.size

    let write_advance t n =
      assert (n <= write_space t);
      if t.wpos + n < t.size then t.wpos <- t.wpos + n
      else t.wpos <- t.wpos + n - t.size

    let read t buff off len =
      assert (len <= read_space t);
      if len > 0 then
        let pre = t.size - t.rpos in
        let extra = len - pre in
        let buffer = Utils.get_some t.buffer in
          if extra > 0 then
            (
              for i = 0 to pre - 1 do
                buff.(i + off) <- buffer.{i + t.rpos}
              done;
              for i = 0 to extra - 1 do
                buff.(i + off + pre) <- buffer.{i}
              done
            )
          else
            for i = 0 to len - 1 do
              buff.(i + off) <- buffer.{i + t.rpos}
            done

    let read_ba t buff off len =
      assert (len <= read_space t);
      if len > 0 then
        let pre = t.size - t.rpos in
        let extra = len - pre in
        let buffer = Utils.get_some t.buffer in
          if extra > 0 then
            (
              for i = 0 to pre - 1 do
                buff.{i + off} <- buffer.{i + t.rpos}
              done;
              for i = 0 to extra - 1 do
                buff.{i + off + pre} <- buffer.{i}
              done
            )
          else
            for i = 0 to len - 1 do
              buff.{i + off} <- buffer.{i + t.rpos}
            done

    let to_array r =
      let len = read_space r in
      let ans = Array.create len 0. in
        read r ans 0 len;
        ans

    (** Compact the ringbuffer, i.e. put all the data at the beginning. *)
    let compact r =
      if r.size > 0 then
        (* If data is small enough for arrays then use them, otherwise use a
         * bigarray. *)
        let len = read_space r in
        let buffer = Utils.get_some r.buffer in
          if len < Sys.max_array_length / 2 then
            let a = to_array r in
              for i = 0 to len - 1 do
                buffer.{i} <- a.(i)
              done
          else
            (
              let copy_fname = Filename.temp_file "liquidsoap_buffer" "" in
              let copy_fd = Unix.openfile copy_fname [Unix.O_RDWR] 0o600 in
              let copy_ba = Bigarray.Array1.map_file r.fd Bigarray.float32 Bigarray.c_layout true len in
                read_ba r copy_ba 0 len;
                (* Bigarray.Array1.blit copy_ba (Bigarray.Array1.sub buffer 0 len); *)
                for i = 0 to len - 1 do
                  buffer.{i} <- copy_ba.{i}
                done;
                Unix.close copy_fd;
                Unix.unlink copy_fname
            );
          r.rpos <- 0;
          r.wpos <- len

    (** Adds space {i at the end}. You should use [compact] before. *)
    let resize r len =
      if len = 0 then
        (
          r.size <- 0;
          r.buffer <- None;
          r.rpos <- 0;
          r.wpos <- 0;
        )
      else
        (
          r.size <- len;
          r.buffer <- Some (Bigarray.Array1.map_file r.fd Bigarray.float32 Bigarray.c_layout true len);
        )

    let write t buff off len =
      if len > write_space t then
        (
          compact t;
          (* Heuristics in order to avoid growing too often. *)
          let grow =
            max (len - write_space t) (Fmt.samples_of_seconds 0.5)
          in
            resize t (t.size + grow)
        );
      if len > 0 then
        let pre = t.size - t.wpos in
        let extra = len - pre in
        let buffer = Utils.get_some t.buffer in
          if extra > 0 then
            (
              for i = 0 to pre - 1 do
                buffer.{i + t.wpos} <- buff.(i + off)
              done;
              for i = 0 to extra - 1 do
                buffer.{i} <- buff.(i + off + pre)
              done
            )
          else
            for i = 0 to len - 1 do
              buffer.{i + t.wpos} <- buff.(i + off)
            done

    let append r buf =
      let buflen = Array.length buf in
        write r buf 0 buflen;
        write_advance r buflen

    let peek r len =
      (* TODO: optimize this (i.e. use Array.init)? *)
      let ans = Array.make len 0. in
        read r ans 0 len;
        ans

    let take r len =
      let ans = peek r len in
        read_advance r len;
        ans
  end

  type t = (int Queue.t * (string * B.t) array)

  let add buf (q, a) =
    let chans = Array.length buf in
      Queue.add (Array.length buf.(0)) q;
      for c = 0 to chans - 1 do
        B.append a.(c) buf.(c)
      done

  let peek (q, a) =
    let chans = Array.length a in
    let len = Queue.peek q in
      Array.init chans (fun c -> B.peek a.(c) len)

  let take (q, a) =
    let chans = Array.length a in
    let len = Queue.peek q in
      Array.init chans (fun c -> B.take a.(c) len)

  let create () =
    let chans = Fmt.channels () in
      Queue.create (),
      Array.init
        chans
        (fun c ->
           let fname = Filename.temp_file "liquidsoap_buffer" "" in
           let fd = Unix.openfile fname [Unix.O_RDWR] 0o600 in
             ignore (Dtools.Init.at_stop (fun () -> Unix.unlink fname));
             B.create fd
        )
end

module Disk_manyfiles_queue =
struct
  type t = (string * int * (float, Bigarray.float32_elt, Bigarray.c_layout Bigarray.layout) Bigarray.Array2.t)

  let add buf q =
    let chans = Array.length buf in
    let buflen = Array.length buf.(0) in
      if buflen > 0 then
        (
          let fname = Filename.temp_file "liquidsoap_buffer" "" in
          let fd = Unix.openfile fname [Unix.O_RDWR] 0o600 in
          let ba = Bigarray.Array2.map_file fd Bigarray.float32 Bigarray.c_layout true chans buflen in
            for c = 0 to chans - 1 do
              let bufc = buf.(c) in
                for i = 0 to buflen - 1 do
                  Bigarray.Array2.set ba c i bufc.(i)
                done
            done;
            Queue.add (fname, fd, ba) q
        )

  let from_ba ba =
    let chans = Bigarray.Array2.dim1 ba in
    let buflen = Bigarray.Array2.dim2 ba in
      Array.init
        chans
        (fun c ->
           Array.init
             buflen
             (fun i -> Bigarray.Array2.get ba c i)
        )

  let peek q =
    let _, _, ba = Queue.peek q in
      from_ba ba

  let take q =
    let fname, fd, ba = Queue.take q in
    let buf = from_ba ba in
      Unix.close fd;
      Unix.unlink fname;
      buf

  let create () =
    let q = Queue.create () in
    let empty q =
      try
        while true do
          ignore (take q)
        done
      with
        | Queue.Empty -> ()
    in
      (* To clean up the files. *)
      ignore (Dtools.Init.at_stop (fun () -> empty q));
      q
end

(** Accumulate float PCM and generate float_pcm tracks. *)
module Generator =
struct

  type buffer = 
   {
     add : float array array -> unit;
     peek : unit -> float array array;
     take : unit -> float array array
   }

  type t = {
    (* Format *)
    out_freq  : int;
    out_chans : int;
    resample  : float -> float array array -> int -> int -> float array array;
    (* Accumulated input data. *)
    mutable metadata : (int*Frame.metadata) list ; (* ticks   *)
    mutable breaks   : int list ;                  (* ticks   *)
    mutable length   : int ;                       (* samples *)
    mutable offset   : int ; (* offset in samples the first buffer *)
    mutable buffers  : buffer ;
  }

  let create_buffers () =
    match conf_raw_buffering#get with
      | "raw" ->
          let queue = Raw_queue.create () in
            {
              add = (fun buf -> Raw_queue.add buf queue);
              peek = (fun () -> Raw_queue.peek queue);
              take = (fun () -> Raw_queue.take queue)
            }
      | "disk" ->
          let queue = Disk_ringbuffer_queue.create () in
            {
              add = (fun buf -> Disk_ringbuffer_queue.add buf queue);
              peek = (fun () -> Disk_ringbuffer_queue.peek queue);
              take = (fun () -> Disk_ringbuffer_queue.take queue)
            }
      | "disk_noring" ->
          let queue = Disk_queue.create () in
            {
              add = (fun buf -> Disk_queue.add buf queue);
              peek = (fun () -> Disk_queue.peek queue);
              take = (fun () -> Disk_queue.take queue)
            }
      | "disk_manyfiles" ->
          let queue = Disk_manyfiles_queue.create () in
            {
              add = (fun buf -> Disk_manyfiles_queue.add buf queue);
              peek = (fun () -> Disk_manyfiles_queue.peek queue);
              take = (fun () -> Disk_manyfiles_queue.take queue)
            }
      | _ ->
          (* TODO: do not silently do this when the value is not "default". *)
          let queue = Queue.create () in
            {
              add = (fun buf -> Queue.add buf queue);
              peek = (fun () -> Queue.peek queue);
              take = (fun () -> Queue.take queue)
            }


  let create ?(out_freq = Fmt.samples_per_second())
             ?(out_chans = Fmt.channels()) () =
    let conv = Audio_converter.Samplerate.create out_chans in
      {
        out_freq  = out_freq ;
        out_chans = out_chans ;
        metadata  = [] ;
        breaks    = [] ;
        length    = 0 ;
        offset    = 0 ;
        buffers   = create_buffers () ;
        resample  = Audio_converter.Samplerate.resample conv ;
      }

  let length b = b.length

  let remaining abg =
    match abg.breaks with
      | a :: _ -> a
      | _ -> Fmt.ticks_of_samples abg.length

  let clear abg =
    abg.length <- 0;
    abg.offset <- 0;
    abg.buffers <- create_buffers ();
    abg.metadata <- [];
    abg.breaks <- []

  let feed abg ?(sample_freq = Fmt.samples_per_second ()) buf =
    let buf =
      match Array.length buf with
        | n when n = Fmt.channels () -> buf
        | n when n > Fmt.channels () ->
            (* TODO: is this really what we want here? *)
            Array.sub buf 0 (Fmt.channels ())
        | 1 -> Array.make (Fmt.channels ()) buf.(0)
        | 0 -> failwith "Generator.feed: no channels"
        | _ -> failwith "Generator.feed: not enough channels"
    in
    let buf =
      abg.resample (float abg.out_freq /. float sample_freq) buf
                   0 (Array.length buf.(0))
    in
      abg.length <- abg.length + (Array.length buf.(0)) ;
      abg.buffers.add buf

  let add_metadata abg m =
    abg.metadata <- abg.metadata @ [Fmt.ticks_of_samples abg.length, m]

  let add_break abg =
    abg.breaks <- abg.breaks @ [Fmt.ticks_of_samples abg.length]

  (* Take all data from a frame: breaks, metadata and available audio. *)
  let feed_from_frame abg frame =
    let size = Frame.size frame in
    let samples = Fmt.samples_of_ticks (Frame.position frame) in
      abg.metadata <-
        abg.metadata @
          (List.map
             (fun (p,m) -> Fmt.ticks_of_samples abg.length + p, m)
             (Frame.get_all_metadata frame)) ;
      abg.breaks <-
        abg.breaks @
          (List.map
             (fun p -> Fmt.ticks_of_samples abg.length + p)
             (List.filter (fun x -> x < size) (Frame.breaks frame))) ;
      feed abg ~sample_freq:(Fmt.samples_per_second ())
        (Array.map
           (fun x -> Array.sub x 0 samples)
           (get_float_pcm frame))

  (* Advance metadata and breaks by [pos] samples. *)
  let advance abg pos =
    let pos = Fmt.ticks_of_samples pos in
    let meta = List.map (fun (x,y) -> (x-pos,y)) abg.metadata in
    let breaks = List.map (fun x -> x-pos) abg.breaks in
      abg.metadata <- List.filter (fun x -> fst x >= 0) meta;
      abg.breaks <- List.filter (fun x -> x >= 0) breaks

  (** Remove [len] samples of data. *)
  let rec remove abg len =
    assert (abg.length >= len) ;
    if len>0 then
    let b = abg.buffers.peek () in
      (* Is it enough to advance in the first buffer?
       * Or do we need to consume it completely and go farther in the queue? *)
      if abg.offset + len < Array.length b.(0) then begin
        abg.length <- abg.length - len ;
        abg.offset <- abg.offset + len ;
        advance abg len
      end else
        let removed = Array.length b.(0) - abg.offset in
          ignore (abg.buffers.take ()) ;
          abg.length <- abg.length - removed ;
          abg.offset <- 0 ;
          advance abg removed ;
          remove abg (len-removed)

  (* Fill the frame from the generator's data. *)
  let fill abg frame =
    (* Audio only (for now) so the official unit is the sample. *)
    let buf = get_float_pcm frame in
    let offset = Fmt.samples_of_ticks (Frame.position frame) in
    let buffer_size = Array.length buf.(0) in
    let blit src src_off dst dst_off len =
      for c = 0 to Array.length src - 1 do
        float_blit src.(c) src_off dst.(c) dst_off len
      done
    in
    (* The main loop takes the current offset in the output buffer,
     * and iterates on input buffer chunks. *)
    let rec aux offset =
      (* How much (more) data should be output? *)
      let needed =
        min
          (Fmt.samples_of_ticks (remaining abg))
          (buffer_size - offset)
      in
      let offset_ticks = Fmt.ticks_of_samples offset in
        if needed = 0 then begin
          Frame.add_break frame offset_ticks ;
          if Frame.is_partial frame then
            match abg.breaks with
              | 0::tl -> abg.breaks <- tl
              | [] -> () (* end of stream / underrun ... *)
              | _ -> assert false
        end else
          let block = abg.buffers.peek () in
          let blocklen = Array.length block.(0) - abg.offset in
          let copied = min needed blocklen in
            blit
              block abg.offset
              buf offset
              copied ;
            List.iter
              (fun (p,m) ->
                 if p < Fmt.ticks_of_samples copied then
                   Frame.set_metadata frame (p + offset_ticks) m)
              abg.metadata ;
            advance abg copied ;
            (* Update buffer data -- did we consume a full block? *)
            if blocklen <= needed then begin
              ignore (abg.buffers.take ()) ;
              abg.length <- abg.length - blocklen ;
              abg.offset <- 0
            end else begin
              abg.length <- abg.length - needed ;
              abg.offset <- abg.offset + needed
            end ;
            (* Add more data by recursing on the next block, or finish. *)
            if blocklen < needed then
              aux (offset+blocklen)
            else begin
              Frame.add_break frame (Fmt.ticks_of_samples (offset+needed)) ;
              if Frame.is_partial frame then
                match abg.breaks with
                  | 0::tl -> abg.breaks <- tl
                  | [] -> () (* end of stream / underrun ... *)
                  | _ -> assert false
            end
    in
      aux offset

end

(** Accumulate raw PCM and generate float_pcm tracks. *)
module Generator_from_raw =
struct
  type t =
      {
        generator : Generator.t;
        convert : string -> float array array;
        in_freq :int;
      }

  let create ~channels ~samplesize ~signed ~big_endian ~in_freq ~out_freq =
    let convert src src_off len ratio =
      let dst =
        (* TODO: convert channel number? *)
        Array.init channels (fun _ -> Array.make len 0.)
      in
        ignore
          (
            resample_s16le
              src src_off len signed samplesize big_endian
              ratio dst 0
          );
        dst
    in
    let convert src =
      convert src 0 (String.length src / (2 * channels)) 1.
    in
      {
        generator = Generator.create ~out_freq:(int_of_float out_freq) ();
        convert = convert;
        in_freq = int_of_float in_freq;
      }

  let clear g = Generator.clear g.generator

  let feed g b =
    Generator.feed g.generator ~sample_freq:(g.in_freq) (g.convert b)

  let add_metadata g x = Generator.add_metadata g.generator x

  let add_break g = Generator.add_break g.generator

  let length g = Generator.length g.generator

  let remaining g = Generator.remaining g.generator

  let remove g = Generator.remove g.generator

  let fill g = Generator.fill g.generator
end

(* Sound processing *)

let multiply a off len c =
  for i = 0 to Array.length a - 1 do
    for j = off to off + len - 1 do
      a.(i).(j) <- c *. a.(i).(j)
    done
  done

let blankify a off len =
  for i = 0 to Array.length a - 1 do
    for j = off to off + len - 1 do
      a.(i).(j) <- 0.
    done
  done

let add dst dst_off src src_off len =
  for i = 0 to Array.length dst - 1 do
    for j = 0 to len - 1 do
      dst.(i).(dst_off+j) <- dst.(i).(dst_off+j) +. src.(i).(src_off+j)
    done
  done

let substract y y_off x x_off len =
  for i = 0 to Array.length y - 1 do
    for j = 0 to len - 1 do
      y.(i).(y_off+j) <- y.(i).(y_off+j) -. x.(i).(x_off+j)
    done
  done

let rms a off len =
  let ans = Array.create (Array.length a) 0. in
    for c = 0 to Array.length a - 1 do
      let a_c = a.(c) in
        for i = off to off + len - 1 do
          ans.(c) <- ans.(c) +. a_c.(i) *. a_c.(i)
        done;
      ans.(c) <- sqrt (ans.(c) /. (float len))
    done;
    ans
