let () =
  let tz = -Utils.timezone () in
  Printf.printf "timezone: %d (sec)\n%!" tz;
  Printf.printf "timezone: %+03d%02d\n%!" (tz / 3600) (abs (tz / 60) mod 60);
  Printf.printf "timezone: %s\n%!" (Utils.strftime "%z");
  let std, dst = Utils.timezone_by_name () in
  Printf.printf "tzname = {%s, %s}\n%!" std dst
