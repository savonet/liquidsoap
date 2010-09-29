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

class file_output
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~append ~perm ~dir_perm filename
  ~infallible ~on_start ~on_stop ~autostart
  ~encoder_factory ~kind source =
object (self)

  inherit
    Output.encoded
      ~infallible ~on_start ~on_stop ~autostart
      ~output_kind:"to_file" ~name:filename
      ~content_kind:kind source

  val mutable fd = None
  val mutable encoder = None
  val mutable open_date = 0.
  val mutable current_metadata = None

  method open_file = 
    let mode =
      Open_wronly::Open_creat::
      (if append then [Open_append] else [Open_trunc])
    in
    let filename = Utils.strftime filename in
    let filename = Utils.home_unrelate filename in
    let current_metadata = 
      match current_metadata with
        | Some m -> Hashtbl.find m
        | None -> fun _ -> raise Not_found
    in
    (* Avoid / in metas for filename.. *)
    let current_metadata s =
      Pcre.substitute
        ~pat:"/" ~subst:(fun _ -> "-") 
        (current_metadata s)
    in
    let filename = Utils.interpolate current_metadata filename in
    let chan =
      Utils.mkdir ~perm:dir_perm (Filename.dirname filename) ;
      open_out_gen mode perm filename
    in
      set_binary_mode_out chan true ;
      open_date <- Unix.gettimeofday () ;
      fd <- Some chan

  method output_start =
    assert (fd = None && encoder = None) ;
    let enc = encoder_factory self#id in
    encoder <- Some enc ;
    match current_metadata with
      | Some m -> 
          self#send (enc.Encoder.insert_metadata m) ;
          current_metadata <- None
      | None -> ()

  method output_stop =
    let flush = (Utils.get_some encoder).Encoder.stop () in
      self#send flush ;
      close_out (Utils.get_some fd) ;
      fd <- None ;
      encoder <- None

  method output_reset = ()

  val mutable need_reset = false
  val mutable reopening = false

  method encode frame ofs len =
    let enc = Utils.get_some encoder in
      enc.Encoder.encode frame ofs len

  method send b =
    if fd = None then self#open_file ;
    output_string (Utils.get_some fd) b ;
    if not reopening then
      if need_reset || 
         (Unix.gettimeofday () > reload_delay +. open_date &&
          (Lang.to_bool (Lang.apply ~t:Lang.bool_t reload_predicate []))) then
          begin
            self#log#f 3 "Re-opening output file..." ;
            (* #output_stop can trigger #send,
             * the [reopening] flag avoids loops *)
            reopening <- true ;
            self#output_stop ;
            self#output_start ;
            reopening <- false ;
            need_reset <- false ;
          end

  method insert_metadata m =
    if reload_on_metadata then
     begin
      current_metadata <- Some m ; 
      need_reset <- true ;
      ""
     end 
    else
      (Utils.get_some encoder).Encoder.insert_metadata m

end

let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "output.file"
    (Output.proto @ [
       "append",
       Lang.bool_t,
       Some (Lang.bool false),
       Some "Do not truncate but append in the file if it exists." ;
  
       "perm",
       Lang.int_t,
       Some (Lang.int 0o666),
       Some "Permission of the file if it has to be created, up to umask. \
             You can and should write this number in octal notation: 0oXXX. \
             The default value is however displayed in decimal \
             (0o666 = 6*8^2 + 6*8 + 6 = 438)." ;
  
       "dir_perm",
       Lang.int_t,
       Some (Lang.int 0o777),
       Some "Permission of the directories if some have to be created, \
             up to umask. Although you can enter values in octal notation \
             (0oXXX) they will be displayed in decimal (for instance, \
             0o777 = 7*8^2 + 7*8 + 7 = 511)." ;
  
       "reopen_delay", Lang.float_t, Some (Lang.float 120.),
       Some "Prevent re-opening of the file within that delay, in seconds." ;
  
       "reopen_on_metadata", Lang.bool_t, Some (Lang.bool false),
       Some "Re-open on every new metadata information." ;
  
       "reopen_when", Lang.fun_t [] Lang.bool_t,
       Some (Lang.val_cst_fun [] (Lang.bool false)),
       Some "When should the output file be re-opened." ;

       "",
       Lang.format_t kind,
       None,
       Some "Encoding format." ;

       "",
       Lang.string_t,
       None,
       Some "Filename where to output the stream. \
             Some strftime conversion specifiers are available: \
             <code>%SMHdmY</code>. You can also use <code>$(..)</code> \
             interpolation notation for metadata." ;

       "", Lang.source_t kind, None, None ])
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Output
    ~descr:"Output the source stream in a file."
    (fun p _ ->
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
       (* File settings *)
       let append = Lang.to_bool (List.assoc "append" p) in
       let perm = Lang.to_int (List.assoc "perm" p) in
       let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
       let reload_predicate = List.assoc "reopen_when" p in
       let reload_delay = Lang.to_float (List.assoc "reopen_delay" p) in
       let reload_on_metadata =
         Lang.to_bool (List.assoc "reopen_on_metadata" p)
       in
       (* Main stuff *)
       let format_val = Lang.assoc "" 1 p in
       let format = Lang.to_format format_val in
       let encoder_factory =
         try
           Encoder.get_factory format
         with
           | Not_found ->
               raise (Lang.Invalid_value (format_val,"Unsupported format"))
       in
       let filename = Lang.to_string (Lang.assoc "" 2 p) in
       let source = Lang.assoc "" 3 p in
       let kind = Encoder.kind_of_format format in
         ((new file_output
             filename ~append ~perm ~dir_perm
             ~infallible ~on_start ~on_stop
             ~reload_delay ~reload_predicate ~reload_on_metadata
             ~autostart ~encoder_factory ~kind source):>Source.source))
