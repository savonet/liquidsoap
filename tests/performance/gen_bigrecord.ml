let () =
  let indexes = List.init 5000 string_of_int in
  let methods =
    String.concat "\n"
      (List.map (fun idx -> [%string "let r.a%{idx} = %{idx}"]) indexes)
  in
  let sums =
    String.concat "\n"
      (List.map (fun idx -> [%string "n := n() + r.a%{idx}"]) indexes)
  in
  Printf.printf "%s"
    [%string
      {|
def sum () =
  r = ()
  %{methods}
  n = ref(0)
  %{sums}
end
time("sum of fields (big record)", sum)
exit(0)
  |}]
