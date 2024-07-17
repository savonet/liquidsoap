
(**
  * Functions for manipulating audio buffers and buffer generators.
  * @author Samuel Mimram and David Baelde
  *)

exception Invalid_format
exception Partial_buffer
exception Invalid_argument

let _ =
  Callback.register_exception "mixer_exn_invalid_format" Invalid_format;
  Callback.register_exception "mixer_exn_partial_buffer" Partial_buffer;
  Callback.register_exception "mixer_exn_invalid_argument" Invalid_argument

let audio_buffer_size = 1024*4 (* Warning: critical parameter *)

let _ = Callback.register "mixer_audio_buffer_size" audio_buffer_size

type format =
    {
      channels : int;
      sample_freq : int;
      sample_size : int;
      big_endian : bool;
      signed : bool
    }

external convert_format : format -> string -> string = "ocaml_mixer_convert_format"
external add_buffer : string -> int -> string -> int -> int -> unit = "ocaml_mixer_add_buffer"
external change_volume : string -> int -> int -> float -> unit = "ocaml_mixer_change_volume"
external sine : string -> int -> int -> int -> float -> float = "ocaml_mixer_sine"

type filter_type = Low_pass | High_pass | Band_pass | Notch

external simple_filter : string -> int -> int -> int -> float -> filter_type -> unit = "ocaml_mixer_simple_filter_bytecode" "ocaml_mixer_simple_filter"

module Generator =
struct

  type t =
      {
	mutable length : int ;
	mutable offset : int ;
	mutable buffers : string Queue.t
      }

  let create () = { length=0 ; offset=0 ; buffers=Queue.create () }

  let length abg = abg.length

  let feed abg fmt buf =
    let buf = convert_format fmt buf in
    abg.length <- abg.length + (String.length buf) ;
    Queue.add buf abg.buffers

  let should_be_feeded abg =
    abg.length < audio_buffer_size

  let is_empty abg = abg.length = 0

end

module Buffer =
struct

  type metadata = (string,string) Hashtbl.t

  type t =
      {
	mutable already : int ;
	mutable buffer : string ;
	mutable metadatas : metadata Queue.t
      }

  let format =
    {
      channels = 2;
      sample_freq = 44100;
      sample_size = 16;
      big_endian = false;
      signed = true
    }

  let size = audio_buffer_size
  let length = (float audio_buffer_size)/.(44100.*.2.*.2.)

  let create () = 
    { already=0 ; buffer=String.make size '\000' ; 
      metadatas = Queue.create () }

  let is_partial ab = ab.already < audio_buffer_size

  let already ab = ab.already
  let set_already ab l = ab.already <- l

  let free ab = ab.already <- 0
  let blankify ab =
    try
      String.fill ab.buffer ab.already (size-ab.already) '\000' ;
    with
      | _ -> failwith "Mixer.Buffer.blankify"

  let fill ab abg =
    let rec aux () =
      let needed = audio_buffer_size - ab.already in
        if abg.Generator.length > 0 && needed > 0
        then
          let block = Queue.peek abg.Generator.buffers in
          let blocklen = String.length block - abg.Generator.offset in
            if blocklen <= needed then
              begin
                String.blit 
                  block abg.Generator.offset 
                  ab.buffer ab.already 
                  blocklen ;
                abg.Generator.length <- abg.Generator.length - blocklen ;
                ignore (Queue.take abg.Generator.buffers) ;
                abg.Generator.offset <- 0 ;
                ab.already <- ab.already + blocklen ;
              end
            else 
              begin
                String.blit 
                  block abg.Generator.offset
                  ab.buffer ab.already 
                  needed ;
                abg.Generator.length <- abg.Generator.length - needed ;
                abg.Generator.offset <- abg.Generator.offset + needed ;
                ab.already <- ab.already + needed ;
              end ;
            aux ()
    in
      try aux () with
        | Failure "String.blit" -> failwith "Mixer.Buffer.fill"
	
  let to_string ab = ab.buffer

  exception No_metadata

  let change_volume buf off len f =
    assert (off mod 2 = 0 && len mod 2 = 0) ;
    change_volume buf.buffer off len f

  let add buf1 off1 buf2 off2 len =
    assert (off1 mod 2 = 0 && off2 mod 2 = 0 && len mod 2 = 0) ;
    add_buffer buf1.buffer off1 buf2.buffer off2 len

  let simple_filter buf off len =
    assert (len mod 2 = 0 && off mod 2 = 0) ;
    simple_filter buf.buffer off len

  let sine buf off len freq phi =
    assert (off mod 2 = 0 && len mod 2 = 0) ;
    sine buf.buffer off len freq phi

  let free_metadatas ab = ab.metadatas <- Queue.create ()
  let push_metadata ab m = Queue.add m ab.metadatas 
  let pop_metadata ab = try Queue.pop ab.metadatas with
    | Queue.Empty -> raise No_metadata
  let copy_metadatas ab = Queue.copy ab.metadatas
  let get_metadata ab =
    let q = Queue.copy ab.metadatas in
    let last = ref None in
      begin try
        while true do
          last := Some (Queue.take q)
        done
      with Queue.Empty -> ()
      end ;
      !last

end
