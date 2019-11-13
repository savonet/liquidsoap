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

(** Piped virtual class: open/close pipe,
  * implements metadata interpolation and
  * takes care of the various reload mechanisms. *)

let pipe_proto kind arg_doc =
  Output.proto
  @ [ ( "reopen_delay",
        Lang.float_t,
        Some (Lang.float 120.),
        Some "Prevent re-opening within that delay, in seconds." );
      ( "reopen_on_metadata",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Re-open on every new metadata information." );
      ( "reopen_when",
        Lang.fun_t [] Lang.bool_t,
        Some (Lang.val_cst_fun [] (Lang.bool false)),
        Some "When should the output be re-opened." );
      ("", Lang.format_t kind, None, Some "Encoding format.");
      ( "",
        Lang.string_getter_t (),
        None,
        Some
          ( arg_doc
          ^ " Some strftime conversion specifiers are available: `%SMHdmY`. \
             You can also use `$(..)` interpolation notation for metadata." )
      );
      ("", Lang.source_t kind, None, None) ]

class virtual piped_output p =
  let e f v = f (List.assoc v p) in
  (* Output settings *)
  let autostart = e Lang.to_bool "start" in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let reload_predicate = List.assoc "reopen_when" p in
  let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
  let reload_on_metadata = Lang.to_bool (List.assoc "reopen_on_metadata" p) in
  (* Main stuff *)
  let format_val = Lang.assoc "" 1 p in
  let format = Lang.to_format format_val in
  let encoder_factory =
    try Encoder.get_factory format
    with Not_found ->
      raise (Lang_errors.Invalid_value (format_val, "Unsupported format"))
  in
  let source = Lang.assoc "" 3 p in
  let name = Lang.to_string_getter (Lang.assoc "" 2 p) in
  let name = name () in
  let kind = Encoder.kind_of_format format in
  object (self)
    inherit
      Output.encoded
        ~infallible ~on_start ~on_stop ~autostart ~output_kind:"output.file"
          ~name ~content_kind:kind source

    initializer
    self#register_command "reopen" ~descr:"Re-open the output." (fun _ ->
        self#reopen ; "Done.")

    val mutable encoder = None

    val mutable open_date = 0.

    val mutable current_metadata = None

    method virtual open_pipe : unit

    method virtual write_pipe : string -> int -> int -> unit

    method virtual close_pipe : unit

    method virtual is_open : bool

    method interpolate ?(subst = fun x -> x) s =
      let current_metadata =
        match current_metadata with
          | Some m ->
              fun x -> subst (Hashtbl.find (Meta_format.to_metadata m) x)
          | None ->
              fun _ -> raise Not_found
      in
      Utils.interpolate current_metadata s

    method prepare_pipe =
      self#open_pipe ;
      open_date <- Unix.gettimeofday ()

    method output_start =
      assert ((not self#is_open) && encoder = None) ;
      let enc = encoder_factory self#id in
      let meta =
        match current_metadata with
          | Some m ->
              m
          | None ->
              Meta_format.empty_metadata
      in
      encoder <- Some (enc meta)

    method output_stop =
      if self#is_open then (
        let flush = (Utils.get_some encoder).Encoder.stop () in
        self#send flush ; self#close_pipe ) ;
      encoder <- None

    method output_reset = ()

    val mutable need_reset = false

    val mutable reopening = false

    method encode frame ofs len =
      let enc = Utils.get_some encoder in
      enc.Encoder.encode frame ofs len

    val m = Mutex.create ()

    method reopen : unit =
      Tutils.mutexify m
        (fun () ->
          (self#log)#important "Re-opening output pipe." ;
          (* #output_stop can trigger #send, the [reopening] flag avoids loops *)
          reopening <- true ;
          self#output_stop ;
          self#output_start ;
          reopening <- false ;
          need_reset <- false)
        ()

    method send b =
      if not self#is_open then self#prepare_pipe ;
      Strings.iter self#write_pipe b ;
      if not reopening then
        if
          need_reset
          || Unix.gettimeofday () > reload_delay +. open_date
             && Lang.to_bool (Lang.apply ~t:Lang.bool_t reload_predicate [])
        then self#reopen

    method insert_metadata m =
      if reload_on_metadata then (
        current_metadata <- Some m ;
        need_reset <- true )
      else (Utils.get_some encoder).Encoder.insert_metadata m
  end

(** Out channel virtual class: takes care 
  * of current out channel and writting to
  * it. *)

let chan_proto kind arg_doc =
  [ ( "flush",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Perform a flush after each write." ) ]
  @ pipe_proto kind arg_doc

class virtual chan_output p =
  let flush = Lang.to_bool (List.assoc "flush" p) in
  object (self)
    val mutable chan = None

    method virtual open_chan : out_channel

    method virtual close_chan : out_channel -> unit

    method open_pipe = chan <- Some self#open_chan

    method write_pipe b ofs len =
      let chan = Utils.get_some chan in
      output_substring chan b ofs len ;
      if flush then Stdlib.flush chan

    method close_pipe =
      self#close_chan (Utils.get_some chan) ;
      chan <- None

    method is_open = chan <> None
  end

(** File output *)

class file_output p =
  let filename = Lang.to_string_getter (Lang.assoc "" 2 p) in
  let append = Lang.to_bool (List.assoc "append" p) in
  let perm = Lang.to_int (List.assoc "perm" p) in
  let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
  let on_close = List.assoc "on_close" p in
  let on_close s =
    Lang.to_unit (Lang.apply ~t:Lang.unit_t on_close [("", Lang.string s)])
  in
  object (self)
    inherit piped_output p

    inherit chan_output p

    val mutable current_filename = None

    method open_chan =
      let mode =
        Open_wronly :: Open_creat
        :: (if append then [Open_append] else [Open_trunc])
      in
      let filename = filename () in
      let filename = Utils.strftime filename in
      let filename = Utils.home_unrelate filename in
      (* Avoid / in metas for filename.. *)
      let subst m = Pcre.substitute ~pat:"/" ~subst:(fun _ -> "-") m in
      let filename = self#interpolate ~subst filename in
      Utils.mkdir ~perm:dir_perm (Filename.dirname filename) ;
      let fd = open_out_gen mode perm filename in
      current_filename <- Some filename ;
      set_binary_mode_out fd true ;
      fd

    method close_chan chan =
      close_out chan ;
      on_close (Utils.get_some current_filename) ;
      current_filename <- None
  end

let file_proto kind =
  [ ( "append",
      Lang.bool_t,
      Some (Lang.bool false),
      Some "Do not truncate but append in the file if it exists." );
    ( "perm",
      Lang.int_t,
      Some (Lang.int 0o666),
      Some
        "Permission of the file if it has to be created, up to umask. You can \
         and should write this number in octal notation: 0oXXX. The default \
         value is however displayed in decimal (0o666 = 6*8^2 + 6*8 + 6 = \
         438)." );
    ( "dir_perm",
      Lang.int_t,
      Some (Lang.int 0o777),
      Some
        "Permission of the directories if some have to be created, up to \
         umask. Although you can enter values in octal notation (0oXXX) they \
         will be displayed in decimal (for instance, 0o777 = 7*8^2 + 7*8 + 7 \
         = 511)." );
    ( "on_close",
      Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t,
      Some (Lang.val_cst_fun [("", Lang.string_t, None)] Lang.unit),
      Some
        "This function will be called for each file, after that it is \
         finished and closed. The filename will be passed as argument." ) ]
  @ chan_proto kind "Filename where to output the stream."

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "output.file" (file_proto kind) ~active:true
    ~kind:(Lang.Unconstrained kind) ~category:Lang.Output
    ~descr:"Output the source stream to a file." (fun p _ ->
      (new file_output p :> Source.source))

(** External output *)

class external_output p =
  let process = Lang.to_string_getter (Lang.assoc "" 2 p) in
  let self_sync = Lang.to_bool (List.assoc "self_sync" p) in
  object (self)
    inherit piped_output p

    inherit chan_output p

    method self_sync = self_sync

    method open_chan =
      let process = process () in
      let process = self#interpolate process in
      Unix.open_process_out process

    method close_chan chan =
      try ignore (Unix.close_process_out chan)
      with Sys_error msg when msg = "Broken pipe" -> ()
  end

let pipe_proto kind descr =
  ( "self_sync",
    Lang.bool_t,
    Some (Lang.bool false),
    Some
      "Set to `true` if the process is expected to control the output's \
       latency. Typical example: `ffmpeg` with the `-re` command-line option."
  )
  :: chan_proto kind descr

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "output.external" ~active:true
    (pipe_proto kind "Process to pipe data to.")
    ~kind:(Lang.Unconstrained kind) ~category:Lang.Output
    ~descr:"Send the stream to a process' standard input." (fun p _ ->
      (new external_output p :> Source.source))
