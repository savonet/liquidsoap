let generated_md =
  [
    ("protocols.md", "--list-protocols-md", None);
    ("reference.md", "--list-functions-md", Some "content/reference-header.md");
    ( "reference-extras.md",
      "--list-extra-functions-md",
      Some "content/reference-header.md" );
    ( "reference-deprecated.md",
      "--list-deprecated-functions-md",
      Some "content/reference-header.md" );
    ("settings.md", "--list-settings", None);
  ]

let mk_html f = Pcre.substitute ~pat:"md(?:\\.in)?$" ~subst:(fun _ -> "html") f

let mk_md ?(content = true) f =
  if Pcre.pmatch ~pat:"md\\.in$" f then
    Pcre.substitute ~pat:"\\.in$" ~subst:(fun _ -> "") (Filename.basename f)
  else if content then "content/" ^ f
  else f

let mk_title = Filename.remove_extension

let mk_subst_rule f =
  if Pcre.pmatch ~pat:"md\\.in$" f then (
    let target = mk_md f in
    Printf.printf
      {|
(rule
  (alias doc)
  (deps
    (:subst_md ./subst_md.exe)
    (:in_md content/%s))
  (target %s)
  (action
    (with-stdout-to %%{target}
      (run %%{subst_md} %%{in_md}))))|}
      f target)

let mk_html_rule ~content f =
  Printf.printf
    {|
(rule
  (alias doc)
  (enabled_if (not %%{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target %s)
  (action (run cp %%{no_pandoc} %%{target})))

(rule
  (alias doc)
  (enabled_if %%{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
    (:md %s))
  (target %s)
  (action
    (ignore-outputs
      (run pandoc --syntax-definition=liquidsoap.xml --highlight=pygments %%{md} --metadata pagetitle=%s --template=template.html -o %%{target}))))
|}
    (mk_html f) (mk_md ~content f) (mk_html f) (mk_title f)

let mk_generated_rule (file, option, header) =
  let header_deps, header_action, header_close =
    match header with
      | None -> ("", "", "")
      | Some fname ->
          ( [%string {|(:header %{fname})|}],
            {|(progn (cat %{header}) (echo "\n")|},
            ")" )
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
    List.sort compare
      (List.filter
         (fun f -> Filename.extension f = ".md" || Filename.extension f = ".in")
         (Array.to_list (Sys.readdir (Filename.concat location "content"))))
  in
  List.iter mk_generated_rule generated_md;
  List.iter mk_subst_rule md;
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
