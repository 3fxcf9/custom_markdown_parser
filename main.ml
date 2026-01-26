let parse_mde ?(figures = Hashtbl.create 0) (mde : string) =
  let reg = Config.build_registry () in
  reg.figures <- figures;
  let tokens = Lexer.tokenize mde |> Array.of_list in
  let ast = Parser.parse reg tokens in
  (Registry.render_document reg ast, reg.metadata)

let () =
  let content = In_channel.with_open_bin "test.mde" In_channel.input_all in
  parse_mde content |> fst |> print_endline
