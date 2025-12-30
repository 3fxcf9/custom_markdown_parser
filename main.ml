(* let r = Config.build_registry ();; *)
(**)
(* print_string "Test"; *)
(* print_int @@ List.length r.html_renderers *)

let () =
  (* let example : Ast.node list = *)
  (*   [ *)
  (*     Bold_feature.BoldNode *)
  (*       [ *)
  (*         Ast.TextNode "text "; *)
  (*         Italic_feature.ItalicNode [ Ast.TextNode "hello" ]; *)
  (*       ]; *)
  (*   ] *)
  (* in *)
  let content = In_channel.with_open_bin "test.mde" In_channel.input_all in
  let reg = Config.build_registry () in
  let tokens = Lexer.tokenize content |> Array.of_list in
  let ast = Parser.parse reg tokens in
  Registry.render_document reg ast |> print_string
