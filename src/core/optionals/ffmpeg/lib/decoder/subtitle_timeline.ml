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

type 'data pending = { duration : int; data : 'data }

type 'data t = {
  merge : 'data -> 'data -> 'data;
  delay : int -> 'data -> 'data;
  output : length:int -> 'data option -> unit;
  mutable state : (int * 'data pending option) option;
}

let create ~merge ~delay ~output () = { merge; delay; output; state = None }

let output_pending t ~length pending =
  t.output ~length (Option.map (fun p -> p.data) pending)

let join t pending ~duration data =
  match pending with
    | None -> Some { duration; data }
    | Some p ->
        Some { duration = max p.duration duration; data = t.merge p.data data }

let item t ~position:q ~duration:d data =
  match t.state with
    | None ->
        if q > 0 then t.output ~length:q None;
        t.state <- Some (max q 0, Some { duration = d; data });
        `Ok
    | Some (p, pending) when q > p ->
        output_pending t ~length:(q - p) pending;
        t.state <- Some (q, Some { duration = d; data });
        `Ok
    | Some (p, pending) when q = p ->
        t.state <- Some (p, join t pending ~duration:d data);
        `Ok
    | Some (p, pending) ->
        if q + d <= p then
          `Item_fault
            (Printf.sprintf
               "Subtitle at %d ended before the reached position %d" q p)
        else (
          let shift = p - q in
          t.state <-
            Some (p, join t pending ~duration:(d - shift) (t.delay shift data));
          `Ok)

let advance t r =
  match t.state with
    | None ->
        t.output ~length:r None;
        t.state <- Some (r, None)
    | Some (p, _) when r <= p -> ()
    | Some (p, pending) ->
        output_pending t ~length:(r - p) pending;
        t.state <- Some (r, None)

let flush t =
  match t.state with
    | Some (p, Some { duration; data }) ->
        let length = max duration 1 in
        t.output ~length (Some data);
        t.state <- Some (p + length, None)
    | _ -> ()

let reached t = Option.map fst t.state
