(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

(** Documentation for plugs. *)
module Plug = struct
  type t = {
    name : string;
    description : string;
    mutable items : (string * string) list;
        (** an item with given name and description *)
  }

  let db = ref []

  let create ~doc name =
    let d = { name; description = doc; items = [] } in
    db := d :: !db;
    d

  let add d ~doc name =
    assert (not (List.mem_assoc name d.items));
    d.items <- (name, doc) :: d.items

  let db () = List.sort compare !db

  let print_md print =
    List.iter
      (fun p ->
        Printf.ksprintf print "# %s\n\n%s\n\n" p.name p.description;
        List.iter
          (fun (name, description) ->
            print ("- " ^ name ^ ": " ^ description ^ "\n"))
          p.items;
        print "\n")
      (db ())

  let print_string = print_md
end

(** Documentation for protocols. *)
module Protocol = struct
  type t = {
    name : string;
    description : string;
    syntax : string;
    static : bool;
  }

  let db = ref []

  let add ~name ~doc ~syntax ~static =
    let p = { name; description = doc; syntax; static } in
    db := p :: !db

  let db () = List.sort compare !db

  let print_md print =
    List.iter
      (fun p ->
        let static = if p.static then " This protocol is static." else "" in
        Printf.ksprintf print "### %s\n\n%s\n\nThe syntax is `%s`.%s\n\n" p.name
          p.description p.syntax static)
      (db ())
end

(** Documenentation for values. *)
module Value = struct
  (** Documentation flags. *)
  type flag = [ `Hidden | `Deprecated | `Experimental | `Extra ]

  let string_of_flag : flag -> string = function
    | `Hidden -> "hidden"
    | `Deprecated -> "deprecated"
    | `Experimental -> "experimental"
    | `Extra -> "extra"

  let flag_of_string = function
    | "hidden" -> Some `Hidden
    | "deprecated" -> Some `Deprecated
    | "experimental" -> Some `Experimental
    | "extra" -> Some `Extra
    | _ -> None

  (** Kind of source. *)
  type source =
    [ `Input
    | `Output
    | `Conversion
    | `FFmpegFilter
    | `Track
    | `Audio
    | `Video
    | `MIDI
    | `Visualization
    | `Synthesis
    | `Liquidsoap ]

  type category =
    [ `Source of source
    | `System
    | `File
    | `Math
    | `String
    | `List
    | `Bool
    | `Liquidsoap
    | `Control
    | `Interaction
    | `Other
    | `Filter
    | `None ]

  let categories : (category * string) list =
    [
      (`Source `Input, "Source / Input");
      (`Source `Output, "Source / Output");
      (`Source `Conversion, "Source / Conversions");
      (`Source `FFmpegFilter, "Source / FFmpeg filter");
      (`Source `Track, "Source / Track processing");
      (`Source `Audio, "Source / Audio processing");
      (`Source `Video, "Source / Video processing");
      (`Source `MIDI, "Source / MIDI processing");
      (`Source `Synthesis, "Source / Sound synthesis");
      (`Source `Visualization, "Source / Visualization");
      (`Source `Liquidsoap, "Source / Liquidsoap");
      (`System, "System");
      (`File, "File");
      (`Math, "Math");
      (`String, "String");
      (`List, "List");
      (`Bool, "Bool");
      (`Liquidsoap, "Liquidsoap");
      (`Control, "Control");
      (`Interaction, "Interaction");
      (`Other, "Other");
      (`Filter, "Filter");
      (`None, "Uncategorized");
    ]

  let string_of_category c = List.assoc c categories

  let category_of_string s =
    (* TODO: remove lowercase comparison and correct the standard library *)
    let s = String.lowercase_ascii s in
    let rec aux = function
      | (c, s') :: _ when s = String.lowercase_ascii s' -> Some c
      | _ :: l -> aux l
      | [] -> None
    in
    aux categories

  type argument = {
    arg_type : string;
    arg_default : string option;  (** default value *)
    arg_description : string option;
  }

  type meth = { meth_type : string; meth_description : string option }

  (** Documentation for a function. *)
  type t = {
    typ : string;
    category : category;
    flags : flag list;
    description : string;
    examples : string list;
    arguments : (string option * argument) list;
    methods : (string * meth) list;
  }

  let db = ref []
  let add (name : string) (doc : t Lazy.t) = db := (name, doc) :: !db
  let get name = Lazy.force (List.assoc name !db)

  (** Only print function names. *)
  let print_functions print =
    !db |> List.map fst |> List.sort compare
    |> List.iter (fun f ->
           print f;
           print "\n")

  let print name print =
    let f = get name in
    let reflow ?(indent = 0) ?(cols = 70) s =
      let buf = Buffer.create 1024 in
      (* Did we just see a backtick? *)
      let backtick = ref false in
      (* Are we allowed to reflow? *)
      let protected = ref false in
      (* Length of the current line *)
      let n = ref 0 in
      let indent () =
        for _ = 1 to indent do
          Buffer.add_char buf ' '
        done
      in
      let newline () =
        Buffer.add_char buf '\n';
        indent ();
        n := 0
      in
      let char c =
        Buffer.add_char buf c;
        incr n
      in
      let space () = if !n >= cols then newline () else char ' ' in
      indent ();
      String.iter
        (fun c ->
          if c = '`' then (
            if not !backtick then protected := not !protected;
            backtick := true)
          else backtick := false;
          if (not !protected) && c = ' ' then space ()
          else if c = '\n' then newline ()
          else char c)
        s;
      Buffer.contents buf
    in
    print f.description;
    print "\n\n";
    print "Type: ";
    print f.typ;
    print "\n\n";
    print ("Category: " ^ string_of_category f.category ^ "\n\n");
    List.iter
      (fun e ->
        print "Example:\n\n";
        print e;
        print "\n\n")
      f.examples;
    print "Arguments:\n\n";
    List.iter
      (fun (l, a) ->
        let l = Option.value ~default:"(unlabeled)" l in
        print (" * " ^ l ^ " : " ^ a.arg_type ^ "\n");
        Option.iter (fun d -> print (reflow ~indent:5 d)) a.arg_description;
        print "\n\n")
      f.arguments;
    if f.methods <> [] then (
      print "Methods:\n\n";
      List.iter
        (fun (l, m) ->
          print (" * " ^ l ^ " : " ^ m.meth_type ^ "\n");
          Option.iter (fun d -> print (reflow ~indent:5 d)) m.meth_description;
          print "\n\n")
        f.methods)

  let to_json () : Json.t =
    let functions =
      !db |> List.map (fun (f, d) -> (f, Lazy.force d)) |> List.sort compare
    in
    let l =
      List.map
        (fun (l, f) ->
          let arguments =
            List.map
              (fun (l, a) ->
                ( Option.value ~default:"" l,
                  `Assoc
                    [
                      ("type", `String a.arg_type);
                      ( "default",
                        Option.fold ~none:`Null
                          ~some:(fun d -> `String d)
                          a.arg_default );
                      ( "description",
                        `String (Option.value ~default:"" a.arg_description) );
                    ] ))
              f.arguments
          in
          let arguments = `Assoc arguments in
          let methods =
            List.map
              (fun (l, m) ->
                ( l,
                  `Assoc
                    [
                      ("type", `String m.meth_type);
                      ( "description",
                        `String (Option.value ~default:"" m.meth_description) );
                    ] ))
              f.methods
          in
          let methods = `Assoc methods in
          ( l,
            `Assoc
              [
                ("type", `String f.typ);
                ("category", `String (string_of_category f.category));
                ( "flags",
                  `Tuple
                    (List.map string_of_flag f.flags
                    |> List.map (fun s -> `String s)) );
                ("description", `String f.description);
                ("examples", `Tuple (List.map (fun s -> `String s) f.examples));
                ("arguments", arguments);
                ("methods", methods);
              ] ))
        functions
    in
    `Assoc l

  let print_functions_md ?(extra = true) print =
    let functions =
      !db
      |> List.map (fun (f, d) -> (f, Lazy.force d))
      |> List.sort compare
      |> List.filter (fun (_, d) ->
             not (List.mem `Hidden d.flags || List.mem `Deprecated d.flags))
    in
    let categories =
      categories |> List.map (fun (c, s) -> (s, c)) |> List.sort compare
    in
    List.iter
      (fun (category_name, category) ->
        print ("## " ^ category_name ^ "\n\n");
        let functions =
          List.filter
            (fun (_, d) ->
              d.category = category && (extra || not (List.mem `Extra d.flags)))
            functions
        in
        List.iter
          (fun (f, d) ->
            print ("### `" ^ f ^ "`\n\n");
            print d.description;
            print "\n\n";
            print "Type:\n\n```\n";
            print d.typ;
            print "\n```\n\n";
            List.iter
              (fun e ->
                print "Example:\n\n";
                Printf.ksprintf print "```liquidsoap\n%s\n```\n\n" e)
              d.examples;
            print "Arguments:\n\n";
            List.iter
              (fun (l, a) ->
                let l = Option.value ~default:"(unlabeled)" l in
                let t = a.arg_type in
                let d =
                  match a.arg_default with
                    | None -> ""
                    | Some d -> ", which defaults to `" ^ d ^ "`"
                in
                let s =
                  match a.arg_description with None -> "" | Some s -> ": " ^ s
                in
                Printf.ksprintf print "- `%s` (of type `%s`%s)%s\n" l t d s)
              d.arguments;
            if d.methods <> [] then (
              print "Methods:\n\n";
              List.iter
                (fun (l, m) ->
                  let t = m.meth_type in
                  let s =
                    match m.meth_description with
                      | None -> ""
                      | Some s -> ": " ^ s
                  in
                  Printf.ksprintf print "- `%s` (of type `%s`)%s\n" l t s)
                d.methods);
            if List.mem `Experimental d.flags then
              print "\nThis function is experimental.\n";
            print "\n")
          functions)
      categories

  let print_emacs_completions print =
    let functions =
      !db
      |> List.map (fun (f, d) -> (f, Lazy.force d))
      |> List.sort compare
      |> List.filter (fun (_, d) ->
             not (List.mem `Hidden d.flags || List.mem `Deprecated d.flags))
    in
    print "(defconst liquidsoap-completions '(\n";
    List.iter
      (fun (name, f) ->
        Printf.ksprintf print
          "#(\"%s\" 0 1 (:type \"%s\" :description \"%s\"))\n" name f.typ
          (String.escaped f.description))
      functions;
    print "))\n\n"
end
