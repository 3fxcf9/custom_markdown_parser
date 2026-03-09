let () =
  let content = In_channel.with_open_bin "test.mde" In_channel.input_all in
  let html, toc, _ = Mde_parser.parse_mde content in
  print_endline (toc ^ html)
