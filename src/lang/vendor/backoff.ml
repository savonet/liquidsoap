(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2021, Sudha Parimala <sudharg247@gmail.com>
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t = int

let single_mask = Bool.to_int (Domain.recommended_domain_count () = 1) - 1
let bits = 5
let max_wait_log = 30 (* [Random.bits] returns 30 random bits. *)
let mask = (1 lsl bits) - 1

let create ?(lower_wait_log = 4) ?(upper_wait_log = 17) () =
  assert (
    0 <= lower_wait_log
    && lower_wait_log <= upper_wait_log
    && upper_wait_log <= max_wait_log);
  (upper_wait_log lsl (bits * 2))
  lor (lower_wait_log lsl bits) lor lower_wait_log

let get_upper_wait_log backoff = backoff lsr (bits * 2)
let get_lower_wait_log backoff = (backoff lsr bits) land mask
let get_wait_log backoff = backoff land mask

let reset backoff =
  let lower_wait_log = get_lower_wait_log backoff in
  backoff land lnot mask lor lower_wait_log

let once backoff =
  let wait_log = get_wait_log backoff in
  let wait_mask = (1 lsl wait_log) - 1 in
  let t = Random.bits () land wait_mask land single_mask in
  for _ = 0 to t do
    Domain.cpu_relax ()
  done;
  let upper_wait_log = get_upper_wait_log backoff in
  let next_wait_log = Int.min upper_wait_log (wait_log + 1) in
  backoff lxor wait_log lor next_wait_log

let default = create ()
