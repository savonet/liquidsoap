
open Dtools
open Types

let log = Log.log ~label:"root"
let logl = Log.logl ~label:"root"

let uptime =
  let base = Unix.time () in
    fun () ->
      (Unix.time ()) -. base

(* The skip is stored and performed only at some point of the main loop. *)
let should_skip = ref false
let skip () =
  log 2 "Skip stored ..." ;
  should_skip := true

(* We maintain a list of the 10 last metadatas *)
let add_metadata, get_metadatas =
  let metadatas = Queue.create () in
  let mlock = Tutils.Mutex.create "Root.mlock" in
  let add m =
    Mutex.lock mlock ;
    Queue.add m metadatas ;
    logl 3 (lazy (Log.f "New metadata: %S."
                    (Request.short_string_of_metadata m))) ;
    if Queue.length metadatas > 10 then ignore (Queue.take metadatas) ;
    Mutex.unlock mlock
  in
  let get =
    Tutils.mutexify mlock
      (fun () ->
        let q = Queue.create () in
          Queue.iter (fun m -> Queue.add (Hashtbl.copy m) q) metadatas ;
          q )
  in
    add,get

(* Output calls scheduling.
  The main thread (root) gets WAV buffers from the scheduler. Then it tells
  the output modules to proceed. They are called *concurently*. The main thread
  waits for all the output modules to finish and starts a new round.

  With more details: each module has a counter [oldseq]; the root knows the
  number of [expected_reads], which is the number of modules; a counter
  [reads] contains the number of modules which have finished in the current
  round; the current round is identified by the counter [sequence].

  Root starts a new round by increasing [sequence].
  Then the modules, waiting in [output_before], start working.
  When a module finishes, it increases [reads].
  When [reads] is equal to [expected_reads], the root starts to
  get ready a new buffer.

  Finally, everything is done using conditions, so signals are sent
  when needed, to the root using [cwrite] or to the modules using
  [cread]. *)

let lock = Mutex.create ()
let cread = Condition.create ()
let cwrite = Condition.create ()
let reads = ref 0
let expected_reads = ref 0
let sequence = ref 0
let shutdown = ref false

let server_before () =
  Mutex.lock lock ;
  reads := 0 ;
  incr sequence ;
  Condition.broadcast cread ;
  Mutex.unlock lock

let output_before oldseq =
  Mutex.lock lock ;
  while !oldseq = !sequence do
    Condition.wait cread lock ;
  done ;
  Mutex.unlock lock

let output_after oldseq =
  Mutex.lock lock ;
  incr oldseq ;
  incr reads ;
  Condition.signal cwrite ;
  Mutex.unlock lock

let server_after () =
  Mutex.lock lock ;
  while !reads <> !expected_reads do
    Condition.wait cwrite lock
  done ;
  Mutex.unlock lock

(* TODO a copy would allow server and outputs to run concurrently ...
 * and could avoid threading problems: is concurrent read safe ? *)
let output_loop wav f =
  let oldseq = ref 0 in
    while not !shutdown do
      output_before oldseq ;
      log 4 "Begin output" ;
      f wav ;
      log 4 "End output" ;
      output_after oldseq
    done

(** Main loop. *)

let start scheduler =

  let wav = Mixer.Buffer.create () in
  let was_unchanged = ref false in
  let old_offset = ref 0 in

  let outputs = Output.list_from_conf () in
    expected_reads := List.length outputs ;
    reads := !expected_reads ;

    log 3 "Initializing the scheduler ..." ;
    scheduler#wake_up ;

    while not scheduler#is_ready do
      log 3 "Waiting for scheduler to be ready..." ;
      Unix.sleep 1 ;
    done ;

    log 3 "Launching the outputs..." ;
    List.iter
      (fun output -> ignore (Tutils.create output (output_loop wav) "output"))
      outputs ;

    Mixer.Buffer.blankify wav ;
    log 3 "Broadcast starts up !" ;

    while not !shutdown do

      server_after () ;

      (** Read its metadatas. *)

      log 4 "Updating metadatas" ;
      begin
	try
	  while true do add_metadata (Mixer.Buffer.pop_metadata wav) done
	with
	  | Mixer.Buffer.No_metadata -> ()
      end ;

      (* Fill the next sound frame.
       * The scheduler is supposed to be able to add audio data to the buffer.
       *
       * The end of track is encoded as a deny to fill completely the
       * buffer. In this loop, we don't care about the end of tracks,
       * but they are taken into account in some operators, e.g the switchs.
       *
       * It should never happen that nothing is added to the buffer,
       * in other words we forbid empty tracks.
       * The only exception to this rule is when the buffer was already
       * empty, because it is necessary to distinguish:
       *  - an end of track at the end of a buffer;
       *  - a normal feed of the end of a buffer.
       * For an empty buffer, two denials are forbidden. *)

      Mixer.Buffer.free wav ;
      old_offset := 0 ;
      while Mixer.Buffer.is_partial wav do
	logl 4 (lazy (Log.f "Feeding from %d" !old_offset)) ;
	scheduler#get wav ;
	let offset = Mixer.Buffer.already wav in
          if !old_offset = offset then
	    if !was_unchanged || offset <> 0
	    then ( failwith "Scheduler died !" )
	    else ( log 3 "Unchanged buffer !" ; was_unchanged := true )
	  else
	    was_unchanged := false ;
	  old_offset := offset ;
      done ;

      (** Output the frame *)

      log 4 "Output.send..." ;
      server_before () ;

      (** Now we can safely skip the current track if necessary. *)

      if !should_skip then
        scheduler#abort_track ;
      should_skip := false ;
      was_unchanged := false

    done ;

    scheduler#sleep
