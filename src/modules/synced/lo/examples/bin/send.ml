let () =
  let addr = Lo.Address.default () in
  Lo.send addr "/1/fader1" [`Float 0.5]
