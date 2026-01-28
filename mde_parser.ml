let parse_mde ?(figures = Hashtbl.create 0) (mde : string) =
  let reg = Config.build_registry () in
  reg.figures <- figures;
  let tokens = Lexer.tokenize mde |> Array.of_list in
  let ast = Parser.parse reg tokens in
  (Registry.render_document reg ast, reg.metadata)
