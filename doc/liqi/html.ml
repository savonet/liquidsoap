open Liqi
open Printf

module S = Set.Make (String)

let add_snippet,regenerate_snippet_index =
  let i = ref None in
  let get () =
    let f = open_in "scripts/index.txt" in
    let rec loop s =
      match try Some (input_line f) with _ -> None with
        | Some l ->
            begin match
              if l<>"" && l.[0]='*' then
                let i = String.index_from l 0 '"' in
                let j = String.index_from l (i+1) '"' in
                let _ = String.index_from l j ':' in
                  Some (String.sub l (i+1) (j-i-1))
              else
                None
            with
              | Some l -> loop (S.add l s)
              | None -> loop s
            end
        | None -> i := Some s
    in
      loop S.empty ;
      close_in f
  in
  let get () =
    match !i with
      | None ->
          (try get () with Sys_error _ -> i := Some S.empty) ;
          begin match !i with
            | Some l -> l
            | None -> assert false
          end
      | Some l -> l
  in
  let set () =
    if !i<>None then
      let f = open_out "scripts/index.txt" in
        S.iter
          (fun l ->
             let html = Filename.chop_extension l ^ ".html" in
               fprintf f "* %S:%s\n" l html)
          (get ()) ;
        close_out f
  in
    (fun filename -> i := Some (S.add filename (get ()))),
    set

(** {Rewriting} *)

let r_quotes = Str.regexp "\""

(* Substitutions to be applied always, even in code. *)
let r_subst_pre =
  List.map (fun (r,s) -> Str.regexp r, s)
    [
      "<", "&lt;";
      ">", "&gt;"
    ]

(** Substitutions to be applied only on text. *)
let r_subst =
  List.map (fun (r,s) -> Str.regexp r, s)
    [
      "&", "&amp;";
      "--", "&ndash;"
    ]

let opening_quotes = ref false
let rewrite pre s =
  let s =
    if pre then s else
      Str.global_substitute
        r_quotes
        (fun s ->
           opening_quotes := not !opening_quotes;
           if !opening_quotes then "“" else "”")
        (List.fold_left (fun s (r,t) -> Str.global_replace r t s) s r_subst)
  in
    List.fold_left (fun s (r,t) -> Str.global_replace r t s) s r_subst_pre

let (!) = rewrite false
let (!!) = rewrite true

let rec print_line f l =
  List.iter
    (function
       | Space -> fprintf f " "
       | Word s -> fprintf f "%s" !s
       | Code s -> fprintf f "<code>%s</code>" !!s
       | HRef (txt,url) ->
           begin
             (* This is a quick help for admins who want to check links. *)
             let f = open_out_gen [Open_append;Open_creat] 0o644 "links.txt" in
               fprintf f "%s\n" url ; close_out f
           end ;
           fprintf f "<a href=\"%s\">%s</a>" url !txt
       | Em l -> fprintf f "<em>%a</em>" print_line l
       | Bf l -> fprintf f "<b>%a</b>" print_line l)
    l

let print_snippet f title snippet =
  fprintf f "\
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \
                    \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
<head>
  <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />
  <title>Liquidsoap :: Audio Stream Generation</title>
  <link href=\"../style.css\" type=\"text/css\" rel=\"stylesheet\" />
  <link href=\"../homepage.css\" type=\"text/css\" rel=\"stylesheet\" />
</head>
<body>
  <div id=\"wrapper\">
    <div id=\"header\">
      <div id=\"logo\">
        <h1>Liquidsoap</h1>
        <h2>Audio Stream Generation</h2>
      </div>
      <div>
      <ul id=\"menu\">
        <li id=\"menu-about\">
          <a href=\"../index.html\">about</a></li>
        <li id=\"menu-doc-index\">
          <a href=\"../documentation.html\">documentation</a></li>
        <li id=\"menu-doc-api\">
          <a href=\"../reference.html\">API</a></li>
        <li id=\"menu-doc-snippets\">
          <a href=\"index.html\">snippets</a></li> 
        <li id=\"menu-developers\">
          <a href=\"http://savonet.rastageeks.org/\">developpers</a></li>
      </ul>
     </div>
    </div>
    <div id=\"content\">
      <h3>%s</h3>
      <pre>%s</pre>
      <a href=\"%s\">Download</a>
    </div>
    <div>
      <div id=\"footer\"> 2003-2008 Savonet team</div>
    </div>
  </div>
</body>
</html>" !title !!snippet !title

let print_snippet_main ~rel f title snippet =
  fprintf f "\
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \
                    \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
<head>
  <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />
  <title>Liquidsoap :: Audio Stream Generation</title>
  <link href=\"%s../style.css\" type=\"text/css\" rel=\"stylesheet\" />
  <link href=\"%s../homepage.css\" type=\"text/css\" rel=\"stylesheet\" />
</head>
<body>
  <div id=\"wrapper\">
    <div id=\"header\">
      <div id=\"logo\">
        <h1>Liquidsoap</h1>
        <h2>Audio Stream Generation</h2>
      </div>
      <div>
      <ul id=\"menu\">
          <li id=\"menu-about\">
            <a href=\"%s../index.html\">about</a></li>
          <li id=\"menu-download\">
            <a href=\"%s../download.html\">download</a></li>
          <li id=\"menu-support\">
            <a href=\"%s../documentation.html\">documentation</a></li>
          <li id=\"menu-developers\">
            <a href=\"http://savonet.rastageeks.org/\">developpers</a></li>
      </ul>
     </div>
    </div>
    <div id=\"content\">
      <h3>%s</h3>
      <pre>%s</pre>
      <a href=\"%s\">Download</a>
    </div>
    <div>
      <div id=\"footer\"> 2003-2008 Savonet team</div>
    </div>
  </div>
</body>
</html>" rel rel rel rel rel !title !!snippet !title

let print_doc ?(rel="") f =
  let pprinter =
    {
      print_paragraph = (fun f p x -> Printf.fprintf f "<p>\n%a</p>\n" p x);
      print_list = (fun f p x -> Printf.fprintf f "<ul>\n%a</ul>\n" p x);
      print_item = (fun f p x -> Printf.fprintf f "<li>\n%a</li>\n" p x);
      print_line = print_line;
    }
  in
    fun doc -> List.iter
      (function
         | Header (n,None,s) -> fprintf f "<h%d>%s</h%d>\n" n !s n
         | Header (n,Some a,s) ->
             fprintf f "<h%d><a name=%S>%s</a></h%d>\n" n a !s n
         | Paragraph p -> print_paragraph pprinter f p
         | Image (title,url) ->
             fprintf f "<img alt=%S src=\"%s%s\" />" title rel url
         | Antiquote aq -> fprintf f "%s" aq
         | Snippet (Some title,body) ->
             add_snippet title ;
             begin
               let script = open_out ("scripts/"^title) in
                 output_string script body ;
                 close_out script ;
             end ;
             begin
               let html = Filename.chop_extension title ^ ".html" in
               let script = open_out ("scripts/"^html) in
                 begin
                   match rel with
                   (* Yes, this is hacky... *)
                     | "" -> print_snippet script title body 
                     | s -> print_snippet_main ~rel script title body
                 end ;
                 close_out script
             end ;
             fprintf f "<pre>%s</pre>\n" !!body ;
             fprintf f "<div align=\"right\">\n\
                        <a href=\"scripts/%s\">\n\
                          <img class=\"grab\" src=\"%simages/grab.png\" \
                               alt=\"Grab the code!\">\n\
                        </a>\n\
                        </div></p>\n" title rel
         | Snippet (None, body) ->
             fprintf f "<pre>%s</pre>\n" !!body)
      doc ;
    regenerate_snippet_index ()

(** How many time do we need to go up (..) from f to reach the base dir. *)
let path_to_base ?(basedir=Filename.current_dir_name) f =
  (* How many steps ? *)
  let rec n_to_base f =
    let d = Filename.dirname f in
    let f = Filename.basename f in
      if d = basedir then
        0
      else
        if d = Filename.parent_dir_name then
          n_to_base f - 1
        else
          n_to_base d + 1
  in
  (* From number of steps to a path *)
  let rec n_to_path n =
    if n = 0 then "" else
      Filename.concat Filename.parent_dir_name (n_to_path (n-1))
  in
    n_to_path (n_to_base f)

let print ?filename f ?title doc =
  let x =
    match filename with
      | None -> ""
      | Some filename -> path_to_base filename
  in
  fprintf f "
<?xml version=\"1.0\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML \
1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
<head>
  <meta content=\"text/html; charset=UTF-8\" http-equiv=\"content-type\" />
  <title>Liquidsoap :: %s</title>
  <link href=\"%sstyle.css\" type=\"text/css\" rel=\"stylesheet\" />
  <link href=\"%shomepage.css\" type=\"text/css\" rel=\"stylesheet\" />
</head>
<body>
<div id=\"wrapper\">
  <div id=\"header\">
    <div id=\"logo\">
      <h1>Liquidsoap</h1>
      <h2>audio stream generation</h2>
    </div>
  <div>
   <ul id=\"menu\">
     <li id=\"menu-about\">
       <a href=\"%sindex.html\">about</a></li>
     <li id=\"menu-doc-index\">
       <a href=\"%sdocumentation.html\">documentation</a></li>
     <li id=\"menu-doc-api\">
       <a href=\"%sreference.html\">API</a></li>
     <li id=\"menu-doc-snippets\">
       <a href=\"%sscripts/index.html\">snippets</a></li>
     <li id=\"menu-developers\">
       <a href=\"http://savonet.rastageeks.org/\">developpers</a></li>
   </ul>
  </div>
  </div>
  <div id=\"content\"><div>
  %a\
  </div></div>
  <div>
    <div id=\"footer\"> 2003-2008 Savonet team</div>
  </div>
  </div>
</body></html>\n"
    (match title with None -> "" | Some t -> !t)
    x x x x x x
    (print_doc ~rel:"") doc

(** Template for the main Savonet website. *)
let print_main_main ?filename f ?title doc =
  fprintf f "\
<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \
   \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
  <head>
    <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />
    <title>Savonet / Liquidsoap :: Audio Stream Generation</title>
    <link href=\"style.css\" type=\"text/css\" rel=\"stylesheet\" />
    <link href=\"homepage.css\" type=\"text/css\" rel=\"stylesheet\" />
  </head>
  <body>
    <div id=\"wrapper\">
      <div id=\"header\">
        <div id=\"logo\">
          <h1>Liquidsoap</h1>
          <h2>Audio Stream Generation</h2>
        </div>
        <div>
         <ul id=\"menu\">
          <li id=\"menu-about\">
            <a href=\"index.html\">about</a></li>
          <li id=\"menu-download\">
            <a href=\"download.html\">download</a></li>
          <li id=\"menu-support\">
            <a href=\"documentation.html\">documentation</a></li>
          <li id=\"menu-developers\">
            <a href=\"http://savonet.rastageeks.org/\">developpers</a></li>
        </ul>
       </div>
      </div>
      <div id=\"index\">
        %a
      </div>
      <div id=\"sidebar\">
        <div class=\"box box-hear\">
          <h3>Listen to...</h3>
          <p class=\"content\">\
           <a href=\"http://dolebrai.net/\" title=\"Listen to \
            Dolebraï\"><img src=\"images/icons/32/audio.png\" alt=\"Listen to 
            Dolebraï\" /></a>
          Dolebraï is a netradio powered by Liquidsoap, that only plays
          <em>free</em> music, as in <em>free</em> speech!
          </p>
          <p class=\"more\">
            <a href=\"http://dolebrai.net/\">More on Dolebraï...</a>
          </p>
          <div class=\"box-bottom\"></div>
        </div>
        <div class=\"box box-say\">
          <h3>They say...</h3>
          <blockquote>
            <p>It is possible that software is not like anything else, \
              that it is meant to be discarded: \
              that the whole point is to always see it as soap bubble?</p>
          </blockquote>
          <p class=\"more\">
            <cite class=\"more\">
              Alan J. Perlis, 74th Epigram on Programming.
            </cite>
          </p>
          <div class=\"box-bottom\"></div>
        </div>
      </div>
  <div>
    <div id=\"footer\"> 2003-2008 Savonet team</div>
  </div>
  </div>
</body>
</html>
" (print_doc ~rel:"") doc

(** Template for the main Savonet website's frontpage. *)
let print_main ?filename ?(basedir=Filename.current_dir_name) f ?title doc =
  let x =
    match filename with
      | None -> ""
      | Some filename -> path_to_base ~basedir filename
  in
  let menu,version =
    let default =
      Printf.sprintf
        "<li id=\"menu-support\">\
         <a href=\"%sdocumentation.html\">documentation</a></li>" x,
        ""
    in
      match filename with
        | None -> default
        | Some f ->
            begin try
              let version =
                Pcre.get_substring (Pcre.exec ~pat:"doc-([^/]+)" f) 1
              in
                "<li id=\"menu-support\">\
                 <a href=\"documentation.html\">documentation</a></li>\
                 <li id=\"menu-doc-api-www\">\
                 <a href=\"reference.html\">language</a></li>",
                version ^ " "
            with Not_found -> default end
  in
  fprintf f "\
<?xml version=\"1.0\"?>
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
  <head>
    <meta content=\"text/html; charset=UTF-8\" http-equiv=\"content-type\" />
    <title>Savonet / Liquidsoap %s:: Audio Stream Generation</title>
    <div>
      <link href=\"%sstyle.css\" type=\"text/css\" rel=\"stylesheet\" />
      <link href=\"%shomepage.css\" type=\"text/css\" rel=\"stylesheet\" />
    </div>
  </head>
  <body>
    <div id=\"wrapper\">
      <div id=\"header\">
        <div id=\"logo\">
          <h1>Liquidsoap</h1>
          <h2>Audio Stream Generation</h2>
        </div>
       <div>
        <ul id=\"menu\">
          <li id=\"menu-about\">
            <a href=\"%sindex.html\">about</a></li>
          <li id=\"menu-download\">
            <a href=\"%sdownload.html\">download</a></li>
%s
          <li id=\"menu-developers\">
            <a href=\"http://savonet.rastageeks.org/\">developpers</a></li>
        </ul>
       </div>
      </div>
      <div id=\"content\">
         %a
      </div>
      <div>
        <div id=\"footer\"> 2003-2008 Savonet team</div>
      </div>
    </div>
  </body>
</html>" version x x x x menu (print_doc ~rel:x) doc
