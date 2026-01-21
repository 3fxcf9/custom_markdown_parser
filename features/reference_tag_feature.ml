open Lexer
open Ast

let parse_block (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  if pos >= Array.length tokens then None
  else
    match tokens.(pos) with
    | Langle ->
        let rec find_close i =
          if i >= Array.length tokens then None
          else
            match tokens.(i) with
            | Rangle ->
                let inner = Array.sub tokens (pos + 1) (i - pos - 1) in
                let content =
                  Array.fold_left
                    (fun acc t ->
                      acc
                      ^
                      match Lexer.token_to_literal t with " " -> "-" | e -> e)
                    "" inner
                in
                Some (ReferenceTagNode content, i - pos + 1)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 1)
    | _ -> None

let parse_inline = parse_block
let render_html _ _ _ = None
let render_tex _ _ _ = None
