open Lexer
open Ast

let parse_inline (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  if pos >= Array.length tokens
  then None
  else
    match tokens.(pos) with
    | Langle ->
        let rec find_close i =
          if i >= Array.length tokens
          then None
          else
            match tokens.(i) with
            | Newline -> None
            | Rangle ->
                let inner = Array.sub tokens (pos + 1) (i - pos - 1) in
                let content =
                  Array.fold_left
                    (fun acc t ->
                      acc
                      ^
                      match Lexer.token_to_literal t with
                      | " " -> "-"
                      | e -> e)
                    "" inner
                in
                Some (ReferenceTagNode content, i - pos + 1)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 1)
    | _ -> None

let parse_block (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  if pos = 0
  then parse_inline tokens pos _after_reference _reg
  else
    match tokens.(pos - 1) with
    | Newline | Indent _ -> parse_inline tokens pos _after_reference _reg
    | _ -> None

let render_html _ _ _ = None
let render_tex _ _ _ = None
