(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
      (`Source `Input, "Source / Input");
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
      (`Track `Input, "Track / Input");
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
    callbacks : (string * meth) list;
  }

  let db = ref Map.empty

  let add (name : string) (doc : t Lazy.t) =
    let name =
      Re.replace ~all:true ~f:(fun _ -> "null") (Re.Pcre.regexp "^_null") name
    in
    db := Map.add name doc !db

  let get name = Lazy.force (Map.find name !db)
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
            let d = Lazy.force d in
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
    print (title_color "Arguments:\n\n");
    List.iter
      (fun (l, a) ->
        let l = Option.value ~default:"(unlabeled)" l in
        let l = label_color l in
        let default =
          match a.arg_default with
            | Some d -> " (default: " ^ default_color d ^ ")"
            | None -> ""
        in
        print (" * " ^ l ^ " : " ^ type_color a.arg_type ^ default ^ "\n");
        Option.iter (fun d -> print (reflow ~indent:5 d)) a.arg_description;
        print "\n\n")
      (List.stable_sort
         (fun v v' ->
           match (v, v') with
             | (None, _), (None, _) -> 0
             | (None, _), _ -> 1
             | _, (None, _) -> -1
             | (l, _), (l', _) -> Stdlib.compare l l')
         f.arguments);
    if f.methods <> [] then (
      print (title_color "Methods:\n\n");
      List.iter
        (fun (l, m) ->
          print (" * " ^ label_color l ^ " : " ^ type_color m.meth_type ^ "\n");
          Option.iter (fun d -> print (reflow ~indent:5 d)) m.meth_description;
          print "\n\n")
        (List.sort compare f.methods));
    if f.callbacks <> [] then (
      print (title_color "Callbacks:\n\n");
      List.iter
        (fun (l, m) ->
          print (" * " ^ label_color l ^ " : " ^ type_color m.meth_type ^ "\n");
          Option.iter (fun d -> print (reflow ~indent:5 d)) m.meth_description;
          print "\n\n")
        (List.sort compare f.callbacks))

  let to_json () : Json.t =
    !db |> Map.to_seq
    |> Seq.map (fun (l, f) ->
        let f = Lazy.force f in
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
        let methods, callbacks =
          match
            List.map
              (fun m ->
                `Assoc
                  (List.map
                     (fun (l, m) ->
                       ( l,
                         `Assoc
                           [
                             ("type", `String m.meth_type);
                             ( "description",
                               `String
                                 (Option.value ~default:"" m.meth_description)
                             );
                           ] ))
                     m))
              [f.methods; f.callbacks]
          with
            | [x; v] -> (x, v)
            | _ -> assert false
        in
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
            ] ))
    |> List.of_seq
    |> fun l -> `Assoc l

  let print_functions_md ?extra ?deprecated print =
    let should_show ~category d =
      let d = Lazy.force d in
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
    List.iter
      (fun (category_name, category) ->
        print ("## " ^ category_name ^ "\n\n");
        Map.iter
          (fun f d ->
            if should_show ~category d then (
              let d = Lazy.force d in
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
              if d.arguments <> [] then (
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
                      match a.arg_description with
                        | None -> ""
                        | Some s -> ": " ^ s
                    in
                    Printf.ksprintf print "- `%s` (of type `%s`%s)%s\n" l t d s)
                  d.arguments;
                print "\n");
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
                  (List.sort compare d.methods);
                print "\n");
              if d.callbacks <> [] then (
                print "Callbacks:\n\n";
                List.iter
                  (fun (l, m) ->
                    let t = m.meth_type in
                    let s =
                      match m.meth_description with
                        | None -> ""
                        | Some s -> ": " ^ s
                    in
                    Printf.ksprintf print "- `%s` (of type `%s`)%s\n" l t s)
                  (List.sort compare d.callbacks);
                print "\n");
              if List.mem `Experimental d.flags then
                print "This function is experimental.\n\n"))
          !db)
      categories

  let print_emacs_completions print =
    print "(defconst liquidsoap-completions '(\n";
    Map.iter
      (fun name f ->
        let f = Lazy.force f in
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

type doc = {
  main : string list;
  special : [ `Category of string | `Flag of string ] list;
  params : (string option * string) list;
  methods : (string * string) list;
  callbacks : (string * string) list;
}

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
    let rec parse_doc ({ main; special; params; methods; callbacks } as _doc) =
      function
      | [] -> _doc
      | line :: lines -> (
          try
            let sub =
              Re.Pcre.exec
                ~rex:
                  (Re.Pcre.regexp
                     "^\\s*@(category|docof|flag|param|method|argsof)\\s*(.*)$")
                line
            in
            let s = Re.Pcre.get_substring sub 2 in
            match Re.Pcre.get_substring sub 1 with
              | "docof" ->
                  let doc = Value.get s in
                  let main =
                    if doc.description <> "" then doc.description :: main
                    else main
                  in
                  let params =
                    List.filter_map
                      (fun (l, a) ->
                        match a.Value.arg_description with
                          | Some d -> Some (l, d)
                          | None -> None)
                      doc.arguments
                    @ params
                  in
                  let doc_specials =
                    `Category (Value.string_of_category doc.category)
                    :: List.map
                         (fun f -> `Flag (Value.string_of_flag f))
                         doc.flags
                  in
                  parse_doc
                    { _doc with main; special = doc_specials @ special; params }
                    lines
              | "argsof" ->
                  let s, only, except =
                    try
                      let sub =
                        Re.Pcre.exec
                          ~rex:
                            (Re.Pcre.regexp "^\\s*([^\\[]+)\\[([^\\]]+)\\]\\s*$")
                          s
                      in
                      let s = Re.Pcre.get_substring sub 1 in
                      let args =
                        List.filter
                          (fun s -> s <> "")
                          (List.map String.trim
                             (String.split_on_char ','
                                (Re.Pcre.get_substring sub 2)))
                      in
                      let only, except =
                        List.fold_left
                          (fun (only, except) v ->
                            if String.length v > 0 && v.[0] = '!' then
                              ( only,
                                String.sub v 1 (String.length v - 1) :: except
                              )
                            else (v :: only, except))
                          ([], []) args
                      in
                      (s, only, except)
                    with Not_found -> (s, [], [])
                  in
                  let doc = Value.get s in
                  let args =
                    List.filter
                      (fun (n, _) ->
                        match n with
                          | None -> false
                          | Some n -> (
                              match (only, except) with
                                | [], except -> not (List.mem n except)
                                | only, except ->
                                    List.mem n only && not (List.mem n except)))
                      doc.arguments
                  in
                  let args =
                    List.filter_map
                      (fun (n, a) ->
                        Option.map (fun d -> (n, d)) a.Value.arg_description)
                      args
                  in
                  parse_doc { _doc with main; params = args @ params } lines
              | "category" ->
                  parse_doc { _doc with special = `Category s :: special } lines
              | "flag" ->
                  parse_doc { _doc with special = `Flag s :: special } lines
              | "param" ->
                  let sub =
                    Re.Pcre.exec
                      ~rex:(Re.Pcre.regexp "^(~?[a-zA-Z0-9_.]+)\\s*(.*)$")
                      s
                  in
                  let label = Re.Pcre.get_substring sub 1 in
                  let descr = Re.Pcre.get_substring sub 2 in
                  let label =
                    if label.[0] = '~' then
                      Some (String.sub label 1 (String.length label - 1))
                    else None
                  in
                  let rec parse_descr descr lines =
                    match lines with
                      | [] -> raise Not_found
                      | line :: lines ->
                          let line =
                            Re.replace ~all:true
                              ~f:(fun _ -> "")
                              (Re.Pcre.regexp "^ *") line
                          in
                          let n = String.length line - 1 in
                          if line.[n] = '\\' then (
                            let descr = String.sub line 0 n :: descr in
                            parse_descr descr lines)
                          else (
                            let descr = List.rev (line :: descr) in
                            (String.concat "" descr, lines))
                  in
                  let descr, lines = parse_descr [] (descr :: lines) in
                  parse_doc
                    { _doc with params = (label, descr) :: params }
                    lines
              | "method" ->
                  let sub =
                    Re.Pcre.exec
                      ~rex:(Re.Pcre.regexp "^(~?[a-zA-Z0-9_.]+)\\s*(.*)$")
                      s
                  in
                  let label = Re.Pcre.get_substring sub 1 in
                  let descr = Re.Pcre.get_substring sub 2 in
                  parse_doc
                    { _doc with methods = (label, descr) :: methods }
                    lines
              | "callback" ->
                  let sub =
                    Re.Pcre.exec
                      ~rex:(Re.Pcre.regexp "^(~?[a-zA-Z0-9_.]+)\\s*(.*)$")
                      s
                  in
                  let label = Re.Pcre.get_substring sub 1 in
                  let descr = Re.Pcre.get_substring sub 2 in
                  parse_doc
                    { _doc with callbacks = (label, descr) :: callbacks }
                    lines
              | d -> failwith ("Unknown documentation item: " ^ d)
          with Not_found -> parse_doc { _doc with main = line :: main } lines)
    in
    let { main; special; params; methods; callbacks } =
      parse_doc
        { main = []; special = []; params = []; methods = []; callbacks = [] }
        doc
    in
    let main = List.rev main in
    let params =
      List.map
        (fun (l, d) ->
          ( l,
            Value.
              { arg_type = "???"; arg_default = None; arg_description = Some d }
          ))
        (List.rev params)
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
        })
