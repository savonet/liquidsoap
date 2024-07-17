(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class virtual to_file
  ~reload_delay ~reload_predicate ~reload_on_metadata
  ~append ~perm ~dir_perm filename =
object (self)

  method virtual log : Dtools.Log.t
  method virtual output_start : unit
  method virtual output_stop  : unit

  val mutable fd = None
  val mutable open_date = 0.
  val mutable current_metadata = fun _ -> raise Not_found

  method set_metadata m = current_metadata <- m

  method file_output_start =
    assert (fd = None) ;
    let mode =
      Open_wronly::Open_creat::
      (if append then [Open_append] else [Open_trunc])
    in
    let filename = Utils.strftime filename in
    let filename = Utils.home_unrelate filename in
    (* Avoid / in metas for filename.. *)
    let current_metadata  = fun s -> Pcre.substitute ~pat:"/" ~subst:(fun _ -> "-") 
         (current_metadata s) in
    let filename = Utils.interpolate current_metadata filename in
    let chan =
      Utils.mkdir ~perm:dir_perm (Filename.dirname filename) ;
      open_out_gen mode perm filename
    in
      set_binary_mode_out chan true ;
      open_date <- Unix.gettimeofday () ;
      fd <- Some chan

  method file_output_stop =
    match fd with
      | None -> assert false
      | Some v -> close_out v ; fd <- None

  method output_reset = ()

  val mutable need_close = false
  val mutable closing = false

  (** TODO
    * This actually relies on vorbis. After a reset_metadata we know that we can
    * split the file, which is not the case otherwise. But for other encoders
    * a split might be impossible. *)
  method send b =
    output_string (Utils.get_some fd) b ;
    if not closing then
      if need_close then begin
        need_close <- false ;
        self#log#f 3 "Re-opening output file..." ;
        self#file_output_stop ;
        self#file_output_start
      end else
        if Unix.gettimeofday () > reload_delay +. open_date then
          if Lang.to_bool (Lang.apply reload_predicate []) then begin
            self#log#f 3 "Re-opening output file..." ;
            (* output_stop can trigger #send, take care to avoid loops *)
            closing <- true ;
            self#output_stop ;
            self#output_start ;
            closing <- false
          end

  method private on_reset_encoder =
    if reload_on_metadata then need_close <- true

end

let proto = [
  "append",
  Lang.bool_t,
  Some (Lang.bool false),
  Some "Do not truncate but append in the file if it exists." ;

  "perm",
  Lang.int_t,
  Some (Lang.int 0o666),
  Some "Permission of the file if it has to be created, up to umask." ;

  "dir_perm",
  Lang.int_t,
  Some (Lang.int 0o777),
  Some ("Permission of the directories if some have to be created, "^
        "up to umask.") ;

  "reopen_delay", Lang.float_t, Some (Lang.float 120.),
  Some "Prevent re-opening of the file within that delay, in seconds." ;

  "reopen_on_metadata", Lang.bool_t, Some (Lang.bool false),
  Some "Re-open on every new metadata information." ;

  "reopen_when", Lang.fun_t [] Lang.bool_t,
  Some (Lang.val_cst_fun [] (Lang.bool false)),
  Some "When should the output file be re-opened." ;

  "",
  Lang.string_t,
  None,
  Some "Filename where to output the stream. \
        Some strftime conversion specifiers are available: \
        <code>%SMHdmY</code>. You can also use <code>$(..)</code> \
        interpolation notation for metadata." ;
]
