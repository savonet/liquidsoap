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

module Map = Map.Make (struct
  type t = string

  let compare (x : string) (y : string) = compare x y
end)

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
    [ `Input of [ `Active | `Passive ]
    | `Output
    | `Conversion
    | `FFmpegFilter
    | `Track
    | `Audio
    | `Video
    | `MIDI
    | `Visualization
    | `Synthesis
    | `Testing
    | `Fade
    | `Liquidsoap ]

  type category =
    [ `Source of source
    | `Track of source
    | `System
    | `File
    | `Math
    | `String
    | `List
    | `Bool
    | `Getter
    | `Time
    | `Liquidsoap
    | `Metadata
    | `Programming
    | `Interaction
    | `Internet
    | `Configuration
    | `Settings
    | `None ]

  let categories : (category * string) list =
    [
      (`Source (`Input `Active), "Source / Input / Active");
      (`Source (`Input `Passive), "Source / Input / Passive");
      (`Source `Output, "Source / Output");
      (`Source `Conversion, "Source / Conversion");
      (`Source `FFmpegFilter, "Source / FFmpeg filter");
      (`Source `Track, "Source / Track processing");
      (`Source `Audio, "Source / Audio processing");
      (`Source `Video, "Source / Video processing");
      (`Source `MIDI, "Source / MIDI processing");
      (`Source `Synthesis, "Source / Sound synthesis");
      (`Source `Visualization, "Source / Visualization");
      (`Source `Liquidsoap, "Source / Liquidsoap");
      (`Source `Fade, "Source / Fade");
      (`Source `Testing, "Source / Testing");
      (`Track (`Input `Passive), "Track / Input");
      (`Track `Output, "Track / Output");
      (`Track `Conversion, "Track / Conversion");
      (`Track `FFmpegFilter, "Track / FFmpeg filter");
      (`Track `Track, "Track / Track processing");
      (`Track `Audio, "Track / Audio processing");
      (`Track `Video, "Track / Video processing");
      (`Track `MIDI, "Track / MIDI processing");
      (`Track `Synthesis, "Track / Sound synthesis");
      (`Track `Visualization, "Track / Visualization");
      (`Track `Liquidsoap, "Track / Liquidsoap");
      (`Track `Fade, "Track / Fade");
      (`System, "System");
      (`Configuration, "Configuration");
      (`Settings, "Settings");
      (`File, "File");
      (`Math, "Math");
      (`String, "String");
      (`List, "List");
      (`Bool, "Bool");
      (`Getter, "Getter");
      (`Liquidsoap, "Liquidsoap");
      (`Metadata, "Metadata");
      (`Programming, "Programming");
      (`Interaction, "Interaction");
      (`Internet, "Internet");
      (`Time, "Time");
      (`None, "Uncategorized");
    ]

  let string_of_category c = List.assoc c categories

  let category_of_string s =
    let rec aux = function
      | (c, s') :: _ when s = s' -> Some c
      | _ :: l -> aux l
      | [] -> None
    in
    aux categories

  type meth = { meth_type : string; meth_description : string option }

  (** Documentation of an argument:
      - [arg_default]: default value
      - [arg_fields]: the fields of a record argument, listed when at least one
        is documented with [@field]. *)
  type argument = {
    arg_type : string;
    arg_default : string option;
    arg_description : string option;
    arg_fields : (string * meth) list;
  }

  (** Documentation for a function. *)
  type t = {
    typ : string;
    category : category;
    flags : flag list;
    description : string;
    examples : string list;
    arguments : (string option * argument) list;
    methods : (string * meth) list;
    callbacks : (string * meth) list;
    sync_description : string option;
    composition_description : string option;
    composition : (string * meth) list;
  }

  let db = ref Map.empty

  let add (name : string) (doc : t Lazy.Mutexed.t) =
    let name =
      Re.replace ~all:true
        ~f:(fun _ -> "null")
        (Re.Pcre.regexp ("^" ^ Reserved.null))
        name
    in
    db := Map.add name doc !db

  let get name = Lazy.Mutexed.force (Map.find name !db)
  let count () = Map.cardinal !db

  (** Only print function names. *)
  let print_functions print =
    Map.iter
      (fun f _ ->
        print f;
        print "\n")
      !db

  let print_functions_by_category print =
    let categories =
      categories |> List.map (fun (c, s) -> (s, c)) |> List.sort compare
    in
    List.iter
      (fun (category_name, category) ->
        print ("# " ^ category_name ^ "\n\n");
        Map.iter
          (fun f d ->
            let d = Lazy.Mutexed.force d in
            if d.category = category && not (List.mem `Hidden d.flags) then
              print ("- " ^ f ^ "\n"))
          !db;
        print "\n")
      categories

  let colorize = Console.colorize
  let title_color = colorize [`bold]
  let type_color = colorize [`yellow]
  let default_color = colorize [`red; `bold]
  let label_color = colorize [`cyan; `bold]

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
    print (title_color f.description);
    print "\n\n";
    print (title_color "Type: ");
    print f.typ;
    print "\n\n";
    print (title_color "Category: " ^ string_of_category f.category ^ "\n\n");
    if f.flags <> [] then (
      let flags = f.flags |> List.map string_of_flag |> String.concat "," in
      print (title_color "Flags: " ^ flags ^ "\n\n"));
    List.iter
      (fun e ->
        print (title_color "Example:\n\n");
        print e;
        print "\n\n")
      f.examples;
    let print_paragraph title =
      Option.iter (fun s ->
          print (title_color (title ^ ":\n\n"));
          print (reflow ~indent:2 s);
          print "\n\n")
    in
    let print_entry ~bullet ~indent label typ description =
      print (bullet ^ label_color label ^ " : " ^ typ ^ "\n");
      Option.iter
        (fun d ->
          print (reflow ~indent d);
          print "\n")
        description;
      print "\n"
    in
    let print_meths ~bullet ~indent meths =
      List.iter
        (fun (l, m) ->
          print_entry ~bullet ~indent l (type_color m.meth_type)
            m.meth_description)
        (List.sort compare meths)
    in
    let print_section title meths =
      if meths <> [] then (
        print (title_color (title ^ ":\n\n"));
        print_meths ~bullet:" * " ~indent:5 meths)
    in
    print_paragraph "Synchronization" f.sync_description;
    print_paragraph "Composition" f.composition_description;
    print (title_color "Arguments:\n\n");
    List.iter
      (fun (l, a) ->
        let default =
          match a.arg_default with
            | Some d -> " (default: " ^ default_color d ^ ")"
            | None -> ""
        in
        print_entry ~bullet:" * " ~indent:5
          (Option.value ~default:"(unlabeled)" l)
          (type_color a.arg_type ^ default)
          a.arg_description;
        print_meths ~bullet:"     - " ~indent:9 a.arg_fields)
      (List.stable_sort
         (fun v v' ->
           match (v, v') with
             | (None, _), (None, _) -> 0
             | (None, _), _ -> 1
             | _, (None, _) -> -1
             | (l, _), (l', _) -> Stdlib.compare l l')
         f.arguments);
    print_section "Methods" f.methods;
    print_section "Callbacks" f.callbacks;
    print_section "Composition methods" f.composition

  let to_json () : Json.t =
    !db |> Map.to_seq
    |> Seq.map (fun (l, f) ->
        let f = Lazy.Mutexed.force f in
        let meth_to_json m =
          `Assoc
            (List.map
               (fun (l, m) ->
                 ( l,
                   `Assoc
                     [
                       ("type", `String m.meth_type);
                       ( "description",
                         `String (Option.value ~default:"" m.meth_description)
                       );
                     ] ))
               m)
        in
        (* A list, since unlabeled arguments all have the same empty label. *)
        let arguments =
          `Tuple
            (List.map
               (fun (l, a) ->
                 `Assoc
                   [
                     ("label", `String (Option.value ~default:"" l));
                     ("type", `String a.arg_type);
                     ( "default",
                       Option.fold ~none:`Null
                         ~some:(fun d -> `String d)
                         a.arg_default );
                     ( "description",
                       `String (Option.value ~default:"" a.arg_description) );
                     ("fields", meth_to_json a.arg_fields);
                   ])
               f.arguments)
        in
        let methods = meth_to_json f.methods in
        let callbacks = meth_to_json f.callbacks in
        let composition = meth_to_json f.composition in
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
              ("callbacks", callbacks);
              ( "sync_description",
                Option.fold ~none:`Null
                  ~some:(fun s -> `String s)
                  f.sync_description );
              ( "composition_description",
                Option.fold ~none:`Null
                  ~some:(fun s -> `String s)
                  f.composition_description );
              ("composition", composition);
            ] ))
    |> List.of_seq
    |> fun l -> `Assoc l

  let print_functions_md ?extra ?deprecated print =
    let should_show ~category d =
      let d = Lazy.Mutexed.force d in
      (not (List.mem `Hidden d.flags))
      && ((not (deprecated = Some true)) || List.mem `Deprecated d.flags)
      && (deprecated = Some true || not (List.mem `Deprecated d.flags))
      && d.category = category
      && ((not (extra = Some true)) || List.mem `Extra d.flags)
      && ((not (extra = Some false)) || not (List.mem `Extra d.flags))
    in
    let categories =
      categories
      |> List.map (fun (c, s) -> (s, c))
      |> List.filter (fun (_, category) ->
          Map.exists (fun _ d -> should_show ~category d) !db)
      |> List.sort compare
    in
    List.iter
      (fun (category, _) ->
        let slug s =
          let s = String.lowercase_ascii s in
          let r = Str.regexp "[ /]+" in
          Str.global_replace r "-" s
        in
        Printf.ksprintf print "- [%s](#%s)\n" category (slug category))
      categories;
    print "\n";
    let print_paragraph title =
      Option.iter (fun s -> Printf.ksprintf print "%s:\n\n%s\n\n" title s)
    in
    let print_entry ?(default = "") ~bullet label typ description =
      let description =
        Option.fold ~none:"" ~some:(fun s -> ": " ^ s) description
      in
      Printf.ksprintf print "%s`%s` (of type `%s`%s)%s\n" bullet label typ
        default description
    in
    let print_section title meths =
      if meths <> [] then (
        print (title ^ ":\n\n");
        List.iter
          (fun (l, m) ->
            print_entry ~bullet:"- " l m.meth_type m.meth_description)
          (List.sort compare meths);
        print "\n")
    in
    List.iter
      (fun (category_name, category) ->
        print ("## " ^ category_name ^ "\n\n");
        Map.iter
          (fun f d ->
            if should_show ~category d then (
              let d = Lazy.Mutexed.force d in
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
              print_paragraph "Synchronization" d.sync_description;
              print_paragraph "Composition" d.composition_description;
              if d.arguments <> [] then (
                print "Arguments:\n\n";
                List.iter
                  (fun (l, a) ->
                    let default =
                      match a.arg_default with
                        | None -> ""
                        | Some d -> ", which defaults to `" ^ d ^ "`"
                    in
                    print_entry ~default ~bullet:"- "
                      (Option.value ~default:"(unlabeled)" l)
                      a.arg_type a.arg_description;
                    List.iter
                      (fun (l, m) ->
                        print_entry ~bullet:"  - " l m.meth_type
                          m.meth_description)
                      (List.sort compare a.arg_fields))
                  d.arguments;
                print "\n");
              print_section "Methods" d.methods;
              print_section "Callbacks" d.callbacks;
              print_section "Composition methods" d.composition;
              if List.mem `Experimental d.flags then
                print "This function is experimental.\n\n"))
          !db)
      categories

  let print_emacs_completions print =
    print "(defconst liquidsoap-completions '(\n";
    Map.iter
      (fun name f ->
        let f = Lazy.Mutexed.force f in
        if not (List.mem `Hidden f.flags || List.mem `Deprecated f.flags) then (
          let t = String.map (fun c -> if c = '\n' then ' ' else c) f.typ in
          Printf.ksprintf print
            "#(\"%s\" 0 1 (:type \"%s\" :description \"%s\"))\n" name t
            (String.escaped f.description)))
      !db;
    print "))\n\n";
    print "(provide 'liquidsoap-completions)\n"
end

type doc_type = [ `Full | `Argsof of string list ]

(* Documentation collected from a comment, in reverse order of the lines. *)
type doc = {
  main : string list;
  special : [ `Category of string | `Flag of string ] list;
  params : (string option * string) list;
  fields : (string option * (string * string)) list;
  methods : (string * string) list;
  callbacks : (string * string) list;
}

(* A description and the lines after it, joining lines that end with a
   backslash. *)
let rec parse_descr descr lines =
  match lines with
    | [] -> raise Not_found
    | line :: lines ->
        let line =
          Re.replace ~all:true ~f:(fun _ -> "") (Re.Pcre.regexp "^ *") line
        in
        let n = String.length line - 1 in
        if line.[n] = '\\' then (
          let descr = String.sub line 0 n :: descr in
          parse_descr descr lines)
        else (
          let descr = List.rev (line :: descr) in
          (String.concat "" descr, lines))

let tag_regexp =
  Re.Pcre.regexp
    "^\\s*@(category|docof|flag|param|field|method|callback|argsof)\\s*(.*)$"

(* The leading name of a tag's text and the rest. *)
let split_name ?(name = "~?[a-zA-Z0-9_.]+") s =
  let sub = Re.Pcre.exec ~rex:(Re.Pcre.regexp ("^(" ^ name ^ ")\\s*(.*)$")) s in
  (Re.Pcre.get_substring sub 1, Re.Pcre.get_substring sub 2)

(* [~label] names a labeled argument; any other name is the unlabeled one. *)
let argument_label name =
  if name.[0] = '~' then Some (String.sub name 1 (String.length name - 1))
  else None

(* [@docof f]: the description, category, flags and arguments of [f]. *)
let add_docof doc name =
  let f = Value.get name in
  let main =
    if f.description <> "" then f.description :: doc.main else doc.main
  in
  let params =
    List.filter_map
      (fun (l, a) -> Option.map (fun d -> (l, d)) a.Value.arg_description)
      f.arguments
    @ doc.params
  in
  let special =
    `Category (Value.string_of_category f.category)
    :: List.map (fun f -> `Flag (Value.string_of_flag f)) f.flags
    @ doc.special
  in
  { doc with main; special; params }

(* [@argsof f[a, !b]]: the arguments of [f], only [a] or all but [b]. *)
let add_argsof doc s =
  let name, only, except =
    try
      let sub =
        Re.Pcre.exec
          ~rex:(Re.Pcre.regexp "^\\s*([^\\[]+)\\[([^\\]]+)\\]\\s*$")
          s
      in
      let args =
        Re.Pcre.get_substring sub 2
        |> String.split_on_char ',' |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      let except, only = List.partition (fun v -> v.[0] = '!') args in
      let except =
        List.map (fun v -> String.sub v 1 (String.length v - 1)) except
      in
      (Re.Pcre.get_substring sub 1, only, except)
    with Not_found -> (s, [], [])
  in
  let selected = function
    | None -> false
    | Some n -> (only = [] || List.mem n only) && not (List.mem n except)
  in
  let params =
    List.filter_map
      (fun (n, a) ->
        if selected n then Option.map (fun d -> (n, d)) a.Value.arg_description
        else None)
      (Value.get name).arguments
  in
  { doc with params = params @ doc.params }

(* One tag line, and the lines left after it. *)
let add_tag doc tag s lines =
  match tag with
    | "docof" -> (add_docof doc s, lines)
    | "argsof" -> (add_argsof doc s, lines)
    | "category" -> ({ doc with special = `Category s :: doc.special }, lines)
    | "flag" -> ({ doc with special = `Flag s :: doc.special }, lines)
    | "param" ->
        let name, descr = split_name s in
        let descr, lines = parse_descr [] (descr :: lines) in
        ({ doc with params = (argument_label name, descr) :: doc.params }, lines)
    | "field" ->
        let name, descr =
          split_name ~name:"~?[a-zA-Z0-9_]+\\.[a-zA-Z0-9_]+" s
        in
        let i = String.index name '.' in
        let label = argument_label (String.sub name 0 i) in
        let name = String.sub name (i + 1) (String.length name - i - 1) in
        let descr, lines = parse_descr [] (descr :: lines) in
        ({ doc with fields = (label, (name, descr)) :: doc.fields }, lines)
    | "method" ->
        let name, descr = split_name s in
        ({ doc with methods = (name, descr) :: doc.methods }, lines)
    | "callback" ->
        let name, descr = split_name s in
        ({ doc with callbacks = (name, descr) :: doc.callbacks }, lines)
    | d -> failwith ("Unknown documentation item: " ^ d)

(* A line that is not a well-formed tag is part of the description. *)
let rec parse_lines doc = function
  | [] -> doc
  | line :: lines ->
      let doc, lines =
        try
          let sub = Re.Pcre.exec ~rex:tag_regexp line in
          add_tag doc
            (Re.Pcre.get_substring sub 1)
            (Re.Pcre.get_substring sub 2)
            lines
        with Not_found -> ({ doc with main = line :: doc.main }, lines)
      in
      parse_lines doc lines

let parse_doc ~pos doc =
  let doc = String.split_on_char '\n' doc in
  let doc =
    List.map
      (fun x ->
        Re.replace ~all:true ~f:(fun _ -> "") (Re.Pcre.regexp "^\\s*#\\s?") x)
      doc
  in
  if doc = [] then None
  else (
    let { main; special; params; fields; methods; callbacks } =
      parse_lines
        {
          main = [];
          special = [];
          params = [];
          fields = [];
          methods = [];
          callbacks = [];
        }
        doc
    in
    let main = List.rev main in
    let params =
      List.map (fun (l, d) -> (l, Some d)) (List.rev params)
      @ List.filter_map
          (fun (l, _) ->
            if List.mem_assoc l params then None else Some (l, None))
          (List.sort_uniq compare fields)
    in
    let params =
      List.map
        (fun (l, d) ->
          let arg_fields =
            List.filter_map
              (fun (l', (name, d)) ->
                if l = l' then
                  Some
                    ( name,
                      Value.{ meth_type = "???"; meth_description = Some d } )
                else None)
              (List.rev fields)
          in
          ( l,
            Value.
              {
                arg_type = "???";
                arg_default = None;
                arg_description = d;
                arg_fields;
              } ))
        params
    in
    let methods =
      List.map
        (fun (l, d) ->
          (l, Value.{ meth_type = "???"; meth_description = Some d }))
        (List.rev methods)
    in
    let callbacks =
      List.map
        (fun (l, d) ->
          (l, Value.{ meth_type = "???"; meth_description = Some d }))
        (List.rev callbacks)
    in
    let main = String.concat "\n" main in
    let main = Lang_string.unbreak_md main in
    (* let main = String.concat "\n" main in *)
    let category, flags =
      List.fold_left
        (fun (c, f) s ->
          match s with `Category c -> (c, f) | `Flag flag -> (c, flag :: f))
        ("Uncategorized", []) special
    in
    let category = String.trim category in
    let category =
      match Value.category_of_string category with
        | Some c -> c
        | None ->
            failwith
              (Printf.sprintf "Unknown category: %s (%s)." category
                 (Pos.to_string pos))
    in
    let flags =
      let f f =
        match Value.flag_of_string f with
          | Some f -> f
          | None ->
              failwith
                (Printf.sprintf "Unknown flag: %s (%s)." f (Pos.to_string pos))
      in
      List.map f flags
    in
    Some
      Value.
        {
          (* filled in later on *)
          typ = "???";
          category;
          flags;
          description = main;
          (* TODO *)
          examples = [];
          arguments = params;
          methods;
          callbacks;
          sync_description = None;
          composition_description = None;
          composition = [];
        })
