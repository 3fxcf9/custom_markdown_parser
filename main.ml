let () =
  let content = In_channel.with_open_bin "test.mde" In_channel.input_all in
  let reg = Config.build_registry () in
  Hashtbl.replace reg.figures "aaa.svg" "TEST";
  let tokens = Lexer.tokenize content |> Array.of_list in
  let ast = Parser.parse reg tokens in
  Registry.render_document reg ast |> print_string
