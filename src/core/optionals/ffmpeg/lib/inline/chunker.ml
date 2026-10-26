(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

type 'item t = {
  helper : 'item Duration.t;
  output : Producer.output;
  to_data : length:int -> 'item list -> Content.data;
  input_time : 'item -> int option;
}

let create ~faults ~src ~output ~to_data ~pts accessors =
  {
    helper = Duration.create ~start:0 ~convert_ts:false ~src ~faults accessors;
    output;
    to_data;
    input_time =
      (fun item ->
        Option.bind (pts item)
          (Time_base.rescale_int64 ~src ~dst:(Time_base.main ())));
  }

let packets ~faults ~src ~output ~to_data () =
  create ~faults ~src ~output ~to_data ~pts:Avcodec.Packet.get_pts
    {
      Duration.get_ts = Avcodec.Packet.get_dts;
      set_ts = Avcodec.Packet.set_dts;
      get_duration = Avcodec.Packet.get_duration;
    }

let frames ~faults ~src ~output ~to_data () =
  create ~faults ~src ~output ~to_data ~pts:Avutil.Frame.pts
    {
      Duration.get_ts = Avutil.Frame.pts;
      set_ts = Avutil.Frame.set_pts;
      get_duration = Avutil.Frame.duration;
    }

let emit t { Duration.length; items } =
  (match items with
    | first :: _ -> Option.iter t.output.place (t.input_time first)
    | [] -> ());
  Generator.put t.output.buffer.Decoder.generator t.output.field
    (t.to_data ~length items)

let push t item =
  match Duration.push t.helper item with
    | `Chunk chunk -> emit t chunk
    | `Nothing | `Dropped -> ()
    | `Discontinuity reason -> raise (Producer.Source_fault reason)

let flush t = Option.iter (emit t) (Duration.flush t.helper)
