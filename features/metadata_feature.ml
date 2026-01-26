open Ast
open Lexer

let parse_block (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) (reg : Registry.t) =
  let n = Array.length tokens in
  if pos + 2 >= n then None
  else
    match (tokens.(pos), tokens.(pos + 1), tokens.(pos + 2)) with
    | Colon, Colon, Colon ->
        let rec find_close i =
          if i + 2 >= n then None
          else
            match (tokens.(i), tokens.(i + 1), tokens.(i + 2)) with
            | Colon, Colon, Colon ->
                let buf = Buffer.create 32 in
                for j = pos + 3 to i - 1 do
                  Buffer.add_string buf (Lexer.token_to_literal tokens.(j))
                done;
                let content = Buffer.contents buf |> String.trim in
                (match Yaml.of_string content with
                | Ok v -> reg.metadata <- Some v
                | Error (`Msg err) -> failwith ("Parse error: " ^ err));
                Some (EmptyNode, i + 3 - pos)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 3)
    | _ -> None

let parse_inline _ _ _ _ = None
let render_html _ _ _ = None
let render_tex _ _ _ = None
