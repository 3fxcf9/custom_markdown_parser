let () =
  let content = In_channel.with_open_bin "demo.mde" In_channel.input_all in
  Mde_parser.parse_mde content |> fst |> print_endline
