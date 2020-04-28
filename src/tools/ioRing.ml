(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Producer/consumer source utils. *)

class virtual ['a] base ~nb_blocks ~blank =
  let () =
    if nb_blocks < 1 then
      failwith "Buffered I/O requires a non-zero buffer length."
  in
  object
    val buffer = Array.init nb_blocks (fun _ -> blank ())

    val mutable read = 0

    val mutable write = 0

    (* First, consider read and write as positions on an infinite string:
     * We always have read<=write.
     * We can always write.
     * We can read when read<write.
     *
     * Now, let's force that write-read<=N.
     * We can write when read-write<N.
     * We can read when read<write.
     *
     * Now, this restriction allows us to work with a ringbuffer of N cells,
     * and read (resp. write) in cell number read (resp. write) modulo N.
     * If we only keep read and write modulo N, we cannot distinguish
     *  - read=write (i.e., the reader has to wait for the writer)
     *  - read+N=write (i.e., the writer has to wait for the reader)
     * But it is enough to maintain read and write modulo 2N.
     * Indeed, if read<=write and write-read<=N, then:
     *  - read = write iff read mod 2N = write mod 2N
     *  - write-read=N iff (read mod 2N <> read mod 2N and
     *                      read mod N = write mod N) *)

    (* Accesses to read/write must be protected by wait_m. *)
    val wait_m = Mutex.create ()

    val wait_c = Condition.create ()

    (* State of the I/O process:
     *   `Idle when stopped;
     *   `Running (thread ID) when running;
     *   `Crashed when process has crashed.
     *   `Tired while shutting down. *)
    val mutable state = `Idle

    method sourcering_stop =
      match state with
        | `Running id ->
            Mutex.lock wait_m;
            state <- `Tired;
            Mutex.unlock wait_m;

            (* One signal is enough since there is only one half of
             * the process waiting for us, the other half cannot be
             * called concurrently with this method. *)
            Condition.signal wait_c;
            Thread.join id;
            state <- `Idle
        | `Tired | `Idle -> assert false
        | `Crashed -> ()
  end

class virtual ['a] input ~nb_blocks ~blank =
  object (self)
    inherit ['a] base ~nb_blocks ~blank

    method virtual pull_block : 'a -> unit

    method virtual id : string

    method virtual close : unit

    method private sleep = self#sourcering_stop

    method output_get_ready =
      assert (state = `Idle);
      read <- 0;
      write <- 0;
      state <- `Running (Tutils.create (fun _ -> self#writer) () self#id)

    method private writer =
      try
        while true do
          (* Wait for the reader to read the block we fancy, or for shutdown. *)
          Mutex.lock wait_m;
          if
            state <> `Tired
            && read <> write
            && write mod nb_blocks = read mod nb_blocks
          then Condition.wait wait_c wait_m;
          Mutex.unlock wait_m;

          (* Exit or... *)
          if state = `Tired then raise Exit;

          (* ...write a block. *)
          self#pull_block buffer.(write mod nb_blocks);
          Mutex.lock wait_m;
          write <- (write + 1) mod (2 * nb_blocks);
          Mutex.unlock wait_m;
          Condition.signal wait_c
        done
      with
        | Exit -> self#close
        | e ->
            (* We crashed. Let's attempt to leave things in a decent state.
             * Note that the exception should only come from #pull_lock,
             * which is performed outside of critical section, so there's
             * not need to unlock. *)
            self#close;

            (* It is possible that the reader is waiting for us,
             * hence blocking the streaming thread, and consequently
             * the possibility of going to sleep peacefully.
             * Let's resume it, even though he'll get an arbitrary block. *)
            Mutex.lock wait_m;
            write <- (write + 1) mod (2 * nb_blocks);
            state <- `Crashed;
            Mutex.unlock wait_m;
            Condition.signal wait_c;
            raise e

    (* This is meant to be called from #get_frame,
     * so it makes sense to require that #sleep hasn't been called
     * and won't be called before #get_block returns. *)
    method private get_block =
      assert (match state with `Running _ | `Crashed -> true | _ -> false);

      (* Check that the writer still has an advance. *)
      Mutex.lock wait_m;
      if write = read && state <> `Crashed then Condition.wait wait_c wait_m;
      let b = buffer.(read mod nb_blocks) in
      read <- (read + 1) mod (2 * nb_blocks);
      Mutex.unlock wait_m;
      Condition.signal wait_c;
      b
  end

class virtual ['a] output ~nb_blocks ~blank =
  object (self)
    inherit ['a] base ~nb_blocks ~blank

    method virtual id : string

    method virtual push_block : 'a -> unit

    method virtual close : unit

    method output_stop = self#sourcering_stop

    method output_start =
      assert (state = `Idle);
      read <- 0;
      write <- 0;
      state <- `Running (Tutils.create (fun () -> self#reader) () self#id)

    method reader =
      try
        while true do
          (* Wait for the writer to emit the block we fancy, or for shutdown. *)
          Mutex.lock wait_m;
          if state <> `Tired && read = write then Condition.wait wait_c wait_m;
          Mutex.unlock wait_m;

          (* Exit or... *)
          if state = `Tired then raise Exit;

          (* ...read a block. *)
          self#push_block buffer.(read mod nb_blocks);
          Mutex.lock wait_m;
          read <- (read + 1) mod (2 * nb_blocks);
          Mutex.unlock wait_m;
          Condition.signal wait_c
        done
      with
        | Exit -> self#close
        | e ->
            (* We crashed. Let's attempt to leave things in a decent state.
             * Note that the exception should only come from #pull_lock,
             * which is performed outside of critical section, so there's
             * not need to unlock. *)
            self#close;

            (* It is possible that the reader is waiting for us,
             * hence blocking the streaming thread, and consequently
             * the possibility of going to sleep peacefully.
             * Let's resume it, even though he'll get an arbitrary block. *)
            Mutex.lock wait_m;
            read <- (read + 1) mod (2 * nb_blocks);
            state <- `Crashed;
            Mutex.unlock wait_m;
            Condition.signal wait_c;
            raise e

    (* This is meant to be called from #output_send,
     * so it makes sense to require that #sleep hasn't been called
     * and won't be called before #put_block returns. *)
    method put_block (f : 'a -> unit) =
      assert (match state with `Running _ | `Crashed -> true | _ -> false);
      Mutex.lock wait_m;
      if
        read <> write
        && write mod nb_blocks = read mod nb_blocks
        && state <> `Crashed
      then Condition.wait wait_c wait_m;
      Mutex.unlock wait_m;
      f buffer.(write mod nb_blocks);
      Mutex.lock wait_m;
      write <- (write + 1) mod (2 * nb_blocks);
      Mutex.unlock wait_m;
      Condition.signal wait_c
  end
