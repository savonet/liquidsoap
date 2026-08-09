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

let mk_md ?(content = true) f =
  if Pcre.pmatch ~rex:(Pcre.regexp "md\\.in$") f then
    Pcre.substitute ~rex:(Pcre.regexp "\\.in$")
      ~subst:(fun _ -> "")
      (Filename.basename f)
  else if content then "content/" ^ f
  else f

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

let doctests = ref []

let mk_test_rule file =
  let test_name = "test-doc-" ^ Filename.(remove_extension (basename file)) in
  doctests := test_name :: !doctests;
  Printf.printf
    {|
(rule
  (alias %s)
  (package liquidsoap)
  (deps
    (alias_rec ../install)
    (source_tree ../src/libs)
    (:test_liq %s)
  )
  (action (run %%{bin:liquidsoap} --check --no-fallible-check %s))
)
|}
    test_name file file

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
  List.iter mk_test_rule liq

let () =
  Printf.printf
    {|(alias
  (name doctest)
  (deps
    %s))
|}
    (String.concat "\n    "
       List.(
         map (Printf.sprintf "(alias %s)") (sort_uniq Stdlib.compare !doctests)))
