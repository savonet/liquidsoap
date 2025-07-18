module Pcre = Re.Pcre

let generated_md =
  [
    ("protocols.md", "--list-protocols-md --disable-deprecated", None);
    ( "reference.md",
      "--list-functions-md --disable-deprecated",
      Some "content/reference-header.md" );
    ( "reference-extras.md",
      "--no-external-plugins --list-extra-functions-md --disable-deprecated",
      Some "content/reference-header.md" );
    ( "reference-deprecated.md",
      "--list-deprecated-functions-md --enable-deprecated",
      Some "content/reference-header.md" );
    ("settings.md", "--list-settings --disable-deprecated", None);
  ]

let mk_html f =
  Pcre.substitute ~rex:(Pcre.regexp "md(?:\\.in)?$") ~subst:(fun _ -> "html") f

let mk_md ?(content = true) f =
  if Pcre.pmatch ~rex:(Pcre.regexp "md\\.in$") f then
    Pcre.substitute ~rex:(Pcre.regexp "\\.in$")
      ~subst:(fun _ -> "")
      (Filename.basename f)
  else if content then "content/" ^ f
  else f

let mk_title = Filename.remove_extension

let mk_subst_rule f =
  if Pcre.pmatch ~rex:(Pcre.regexp "md\\.in$") f then (
    let target = mk_md f in
    Printf.printf
      {|
(rule
  (alias doc)
  (package liquidsoap)
  (deps
    (:subst_md ./subst_md.exe)
    (:in_md content/%s))
  (target %s)
  (action
    (with-stdout-to %%{target}
      (run %%{subst_md} %%{in_md}))))|}
      f target)

let mk_html_rule ~liq ~content f =
  let liq = liq |> List.map (fun f -> "    " ^ f) |> String.concat "\n" in
  Printf.printf
    {|
(rule
  (alias doc)
  (package liquidsoap)
  (enabled_if (not %%{bin-available:pandoc}))
  (deps (:no_pandoc no-pandoc))
  (target %s)
  (action (run cp %%{no_pandoc} %%{target}))
)

(rule
  (alias doc)
  (package liquidsoap)
  (enabled_if %%{bin-available:pandoc})
  (deps
    liquidsoap.xml
    language.dtd
    template.html
%s
    (:md %s)
  )
  (target %s)
  (action
    (pipe-stdout
      (run pandoc %%{md} -t json)
      (run pandoc-include --directory content/liq)
      (run pandoc -f json --syntax-definition=liquidsoap.xml --highlight=pygments --metadata pagetitle=%s --template=template.html -o %%{target})
    )
  )
)
|}
    (mk_html f) liq (mk_md ~content f) (mk_html f) (mk_title f)

let mk_generated_rule (file, option, header) =
  let header_deps, header_action, header_close =
    match header with
      | None -> ("", "", "")
      | Some fname ->
          ( [%string {|(:header %{fname})|}],
            {|(progn (cat %{header}) (echo "\n")|},
            ")" )
  in
  let header_action =
    if header_action = "" then "" else "\n      " ^ header_action
  in
  let header_close =
    if header_close = "" then "" else "\n      " ^ header_close
  in
  Printf.printf
    {|
(rule
  (alias doc)
  (package liquidsoap)
  (deps
    %s
    (source_tree ../src/libs))
  (target %s)
  (action
    (with-stdout-to %s%s
      (setenv PAGER none
        (run %%{bin:liquidsoap} %s)))))%s
|}
    header_deps file file header_action option header_close

let mk_test_rule file =
  Printf.printf
    {|
(rule
  (alias doctest)
  (package liquidsoap)
  (deps
    (source_tree ../src/libs)
    (:test_liq %s)
  )
  (action (run %%{bin:liquidsoap} --check --no-fallible-check %s))
)
|}
    file file

let mk_html_install f =
  Printf.sprintf {|    (%s as html/%s)|} (mk_html f) (mk_html f)

let rec readdir ?(cur = []) ~location dir =
  List.fold_left
    (fun cur file ->
      let file = Filename.concat dir file in
      if Sys.is_directory (Filename.concat location file) then
        readdir ~cur ~location file
      else file :: cur)
    cur
    (Build_tools.read_files ~location dir)

let () =
  let location = Filename.dirname Sys.executable_name in
  let md =
    Sys.readdir (Filename.concat location "content")
    |> Array.to_list
    |> List.filter (fun f ->
           Filename.extension f = ".md" || Filename.extension f = ".in")
    |> List.sort compare
  in
  let liq =
    Sys.readdir (Filename.concat location "content/liq")
    |> Array.to_list
    |> List.filter (fun f -> Filename.extension f = ".liq")
    |> List.sort compare
    |> List.map (fun f -> "content/liq/" ^ f)
  in
  List.iter mk_generated_rule generated_md;
  List.iter mk_subst_rule md;
  List.iter
    (fun (file, _, _) -> mk_html_rule ~liq ~content:false file)
    generated_md;
  List.iter (mk_html_rule ~liq ~content:true) md;
  List.iter mk_test_rule liq;
  let files =
    List.map
      (fun f -> Printf.sprintf {|    (orig/%s as html/%s)|} f f)
      (readdir ~location:(Filename.concat location "orig") "")
    @ List.map (fun (f, _, _) -> mk_html_install f) generated_md
    @ List.map mk_html_install md
  in
  let files = files |> List.sort compare |> String.concat "\n" in
  Printf.printf
    {|
(install
  (section doc)
  (package liquidsoap)
  (files
%s
  )
)
|}
    files
