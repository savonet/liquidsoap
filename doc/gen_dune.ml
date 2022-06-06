let generated_md =
  [
    ("protocols.md", "--list-protocols-md", None);
    ("reference.md", "--list-functions-md", Some "content/reference-header.md");
    ( "reference-extras.md",
      "--list-extra-functions-md",
      Some "content/reference-header.md" );
    ("settings.md", "--list-settings", None);
  ]

let mk_html f = Pcre.substitute ~pat:"md$" ~subst:(fun _ -> "html") f
let mk_title = Filename.remove_extension

let mk_html_rule ~content f =
  Printf.printf
    {|
(rule
  (alias doc)
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    no-pandoc
    (:md %s%s))
  (target %s)
  (action
    (ignore-outputs
      (system "pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %%{md} --metadata pagetitle=%s --template=template.html -o %s || cp no-pandoc %s"))))
|}
    (if content then "content/" else "")
    f (mk_html f) (mk_title f) (mk_html f) (mk_html f)

let mk_generated_rule (file, option, header) =
  let header_deps, header_action, header_close =
    match header with
      | None -> ("", "", "")
      | Some fname ->
          ([%string {|(:header %{fname})|}], "(progn (cat %{header})", ")")
  in
  Printf.printf
    {|
(rule
  (alias doc)
  (deps
    %s
    (:liquidsoap ../src/bin/liquidsoap.exe))
  (target %s)
  (action
    (with-stdout-to %s
      %s
      (setenv PAGER none
        (run %%{liquidsoap} %s)))))
      %s
|}
    header_deps file file header_action option header_close

let mk_html_install f =
  Printf.sprintf {|(%s as html/%s)|} (mk_html f) (mk_html f)

let rec readdir ?(cur = []) ~location dir =
  List.fold_left
    (fun cur file ->
      let file = Filename.concat dir file in
      if Sys.is_directory (Filename.concat location file) then
        readdir ~cur ~location file
      else file :: cur)
    cur
    (Array.to_list (Sys.readdir (Filename.concat location dir)))

let () =
  let location = Filename.dirname Sys.executable_name in
  let md =
    List.filter
      (fun f -> Filename.extension f = ".md")
      (Array.to_list (Sys.readdir (Filename.concat location "content")))
  in
  List.iter mk_generated_rule generated_md;
  List.iter (fun (file, _, _) -> mk_html_rule ~content:false file) generated_md;
  List.iter (mk_html_rule ~content:true) md;
  Printf.printf
    {|
(install
 (section doc)
 (package liquidsoap)
 (files
    %s
    %s
    %s))
  |}
    (String.concat "\n"
       (List.map
          (fun f -> Printf.sprintf {|(orig/%s as html/%s)|} f f)
          (readdir ~location:(Filename.concat location "orig") "")))
    (String.concat "\n"
       (List.map (fun (f, _, _) -> mk_html_install f) generated_md))
    (String.concat "\n" (List.map mk_html_install md))
