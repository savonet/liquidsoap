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

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("FAIL: " ^ s);
      exit 1)
    fmt

let ok fmt = Printf.ksprintf (fun s -> print_endline ("ok: " ^ s)) fmt
let r = { Pollset.read = true; write = false; except = false }
let w = { Pollset.read = false; write = true; except = false }
let pair () = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
let poke fd = ignore (Unix.write fd (Bytes.of_string "x") 0 1)

let test_registry () =
  let t = Pollset.create () in
  let a, b = pair () in
  if Pollset.mem t a then fail "an unregistered descriptor was reported present";
  Pollset.set t a r;
  if not (Pollset.mem t a) then fail "a registered descriptor was not present";
  Pollset.remove t a;
  if Pollset.mem t a then fail "a removed descriptor was still present";
  Pollset.remove t a;
  Pollset.close t;
  List.iter Unix.close [a; b];
  ok "set, mem and remove agree, and removing twice is accepted"

let test_readiness () =
  let t = Pollset.create () in
  let a, b = pair () in
  let c, d = pair () in
  Pollset.set t a r;
  Pollset.set t c r;
  if Pollset.wait t ~timeout:0. <> [] then
    fail "quiet descriptors were reported";
  poke b;
  (match Pollset.wait t ~timeout:1. with
    | [(fd, i)] when fd = a && i.Pollset.read -> ()
    | l -> fail "expected only the poked descriptor, got %d" (List.length l));
  Pollset.close t;
  List.iter Unix.close [a; b; c; d];
  ok "a ready descriptor is reported and a quiet one is not"

(* The property the kernel-side set exists for: what a wait costs should follow
   what is ready, not what is watched. *)
let test_one_of_many () =
  let t = Pollset.create () in
  let n = 500 in
  let socks = List.init n (fun _ -> pair ()) in
  List.iter (fun (a, _) -> Pollset.set t a r) socks;
  let target, peer = List.nth socks (n / 2) in
  poke peer;
  (match Pollset.wait t ~timeout:1. with
    | [(fd, _)] when fd = target -> ()
    | l ->
        fail "with %d watched and one ready, wait returned %d" n (List.length l));
  Pollset.close t;
  List.iter
    (fun (a, b) ->
      Unix.close a;
      Unix.close b)
    socks;
  ok "with %d watched and one ready, wait returned exactly that one" n

let test_write_and_timeout () =
  let t = Pollset.create () in
  let a, b = pair () in
  Pollset.set t a w;
  (match Pollset.wait t ~timeout:1. with
    | [(fd, i)] when fd = a && i.Pollset.write -> ()
    | _ -> fail "a writable socket was not reported writable");
  Pollset.set t a r;
  let start = Unix.gettimeofday () in
  if Pollset.wait t ~timeout:0.2 <> [] then
    fail "a quiet wait reported something";
  let waited = Unix.gettimeofday () -. start in
  if waited < 0.15 then
    fail "wait returned after %.3fs, before its timeout" waited;
  Pollset.close t;
  List.iter Unix.close [a; b];
  ok "write readiness is reported and a timeout is honoured"

let test_level_triggered () =
  let t = Pollset.create () in
  let a, b = pair () in
  Pollset.set t a r;
  poke b;
  if List.length (Pollset.wait t ~timeout:1.) <> 1 then
    fail "first wait missed it";
  if List.length (Pollset.wait t ~timeout:0.) <> 1 then
    fail "undrained data was reported once only, which is edge-triggered";
  ignore (Unix.read a (Bytes.create 1) 0 1);
  if Pollset.wait t ~timeout:0. <> [] then
    fail "drained data was still reported";
  Pollset.close t;
  List.iter Unix.close [a; b];
  ok "readiness is level-triggered"

let test_peer_close () =
  let t = Pollset.create () in
  let a, b = pair () in
  Pollset.set t a r;
  Unix.close b;
  (match Pollset.wait t ~timeout:1. with
    | [(fd, _)] when fd = a -> ()
    | _ -> fail "a closed peer was not reported");
  Pollset.close t;
  Unix.close a;
  ok "a closed peer is reported"

let () =
  Printf.printf "backend: %s\n%!" (Pollset.backend (Pollset.create ()));
  test_registry ();
  test_readiness ();
  test_one_of_many ();
  test_write_and_timeout ();
  test_level_triggered ();
  test_peer_close ();
  print_endline "all pollset checks passed"
