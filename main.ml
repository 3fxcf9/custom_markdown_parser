(* open Ast *)

let () =
  let content = In_channel.with_open_bin "test.mde" In_channel.input_all in
  let reg = Config.build_registry () in
  let tokens = Lexer.tokenize content |> Array.of_list in
  (* Array.iter Lexer.debug_token tokens; *)
  (* print_newline (); *)
  let ast = Parser.parse reg tokens in
  (* debug_nodes ast; *)
  (* print_newline (); *)
  Registry.render_document reg ast |> print_string
