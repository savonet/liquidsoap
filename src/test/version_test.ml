let () =
  assert (String_utils.Version.of_string "2.0.0~beta1" = ([2; 0; 0], "~beta1"));
  assert (
    String_utils.Version.of_string "2.0.0+git@7e211ffd"
    = ([2; 0; 0], "+git@7e211ffd"));
  assert (
    String_utils.Version.of_string "2.314234~beta1" = ([2; 314234], "~beta1"));
  assert (String_utils.Version.of_string "2.314234" = ([2; 314234], ""));
  assert (String_utils.Version.of_string "2.0.0" = ([2; 0; 0], ""));
  assert (
    String_utils.Version.of_string "2.3.1.4.2.3.4" = ([2; 3; 1; 4; 2; 3; 4], ""));
  assert (String_utils.Version.compare ([2; 0; 0], "") ([2; 0; 0], "~beta1") = 1);
  assert (
    String_utils.Version.compare ([2; 0; 0], "+foo") ([2; 0; 0], "~beta1") = 1);
  assert (
    String_utils.Version.compare ([2; 0; 0], "~beta1") ([2; 0; 0], "+foo") = -1);
  assert (
    String_utils.Version.compare ([2; 0; 0], "~beta1") ([2; 0; 0], "") = -1);
  assert (String_utils.Version.compare ([2; 1; 0], "~beta1") ([2; 0; 0], "") = 1);
  assert (
    String_utils.Version.compare ([2; 1; 0], "~beta1") ([2; 12; 0], "") = -1);
  assert (String_utils.Version.compare ([2; 12; 0], "") ([2; 12; 0], "") = 0);
  assert (
    String_utils.Version.compare ([2; 0; 0], "+foo") ([2; 0; 0], "+bla") = 1);
  assert (
    String_utils.Version.compare ([2; 0; 0], "~bla") ([2; 0; 0], "~foo") = -1);
  assert (String_utils.Version.compare ([2; 0], "") ([2; 0; 0], "") = -1);
  assert (String_utils.Version.compare ([2; 0; 0], "") ([2; 0], "") = 1)
