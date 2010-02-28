(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

module Generator = Generator.From_frames

class resample ~kind ~active ~ratio (source:source) =
object (self)

  (* Hide our child: we'll treat it specially. *)
  inherit source ~name:"resample" kind as super

  method stype = source#stype

  method remaining =
    let rem = source#remaining in
      if rem = -1 then rem else
        int_of_float (float rem *. (ratio ()))

  method abort_track = source#abort_track

  method private wake_up x =
    (* Call super just for the debugging log messages *)
    super#wake_up x ;
    source#get_ready [(self:>source)]
  method private sleep = source#leave (self:>source)

  (* Clock setting: we need total control on our source's flow. *)

  method set_clock =
    let c = Clock.create_known (new Clock.clock self#id) in
      Clock.unify
        self#clock (Clock.create_unknown ~sources:[] ~sub_clocks:[c]) ;
      Clock.unify source#clock c

  (* Actual processing: put data in a buffer until there is enough,
   * then produce a frame using that buffer.
   * Although this is (currently) an audio-only operator,
   * we do everything using Frame and ticks to avoid any confusion,
   * since the Generator uses that convention. *)

  val mutable generator = Generator.create ()

  method is_ready = Generator.length generator > 0 || source#is_ready

  val mutable converter = None

  (* Whenever we need data we call our source, using a special frame.
   * Say A is our self#clock and B is the clock we've assigned to
   * the source. There could be several ways to tick B, it is possible
   * to tick it sometimes even though the source hasn't produced a full
   * frame yet, just like A might tick even though we stopped streaming
   * before the end of the frame. But we find it the simplest policy
   * to align B ticks on [frame] being full.
   * This has the inconvenient that if the resample stops being used
   * in clock A, then clock B and its outputs stop moving. *)

  val frame = Frame.create kind
  val mutable master_time = 0
  val mutable last_slave_tick = 0 (* in master time *)

  method private slave_tick =
    (Clock.get source#clock)#end_tick ;
    source#after_output ;
    Frame.advance frame ;
    last_slave_tick <- (Clock.get self#clock)#get_tick

  method after_output =
    super#after_output ;
    let master_clock = Clock.get self#clock in
      if master_time <> master_clock#get_tick then begin
        if active && last_slave_tick <> master_time then begin
          self#slave_tick ;
          last_slave_tick <- master_time
        end ;
        master_time <- master_clock#get_tick
      end

  method private fill_buffer =
    if Lazy.force Frame.size = Frame.position frame then self#slave_tick ;
    let start = Frame.position frame in
    let stop = source#get frame ; Frame.position frame in
    let ratio = ratio () in
    let content =
      let start = Frame.audio_of_master start in
      let stop  = Frame.audio_of_master stop in
      let content = AFrame.content frame start in
      let converter =
        match converter with
          | Some c -> c
          | None ->
              let c =
                Audio_converter.Samplerate.create (Array.length content)
              in
                converter <- Some c ;
                c
      in
      let len = stop-start in
      let pcm =
        Audio_converter.Samplerate.resample converter ratio content start len
      in
        { Frame.audio = pcm ; video = [||] ; midi = [||] }
    in
    let convert x = int_of_float (float x *. ratio) in
    let metadata =
      List.map
        (fun (i,m) -> convert i, m)
        (List.filter
           (fun (i,_) -> start<=i && stop<i)
           (Frame.get_all_metadata frame))
    in
    let start = convert start in
    let stop = convert stop in
      Generator.feed generator ~metadata content start (stop-start) ;
      if Frame.is_partial frame then Generator.add_break generator

  method private get_frame frame =
    let needed = Lazy.force Frame.size - Frame.position frame in
    let need_fill () =
      if Generator.remaining generator = -1 then
        Generator.length generator < needed
      else
        false
    in
      (* If self#get_frame is called it means that we and our source
       * are ready, and if our source fails we will stop filling,
       * so there's no need to check that it's ready. *)
      while need_fill () do
        self#fill_buffer
      done ;
      Generator.fill generator frame

end

let () =
  let k = Lang.audio_any in
  Lang.add_operator "stretch" (* TODO better name *)
    [
      "ratio", Lang.float_getter_t 1, None,
        Some "A value higher than 1 means slowing down.";
      "active", Lang.bool_t, Some (Lang.bool true),
        Some "The active behavior is to keep ticking the child's clock \
              when the operator is not streaming. Otherwise the child's clock \
              is strictly based on what is streamed off the child source, \
              which results in time-dependent active sources to be frozen \
              when that source is stopped." ;
      "", Lang.source_t (Lang.kind_type_of_kind_format ~fresh:2 k), None, None
    ]
    ~kind:k
    ~category:Lang.SoundProcessing
    ~descr:"Slow down or accelerate an audio stream by \
            stretching (sounds lower) or squeezing it (sounds higher)."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let ratio = Lang.to_float_getter (f "ratio") in
       let active = Lang.to_bool (f "active") in
         new resample ~kind ~active ~ratio src)
