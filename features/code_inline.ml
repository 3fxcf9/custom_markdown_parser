open Ast
open Lexer

let parse_block _ _ _ _ = None

let parse_inline (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  let n = Array.length tokens in
  if pos + 1 >= n
  then None
  else
    match tokens.(pos) with
    | Backtick ->
        let rec find_close i =
          if i >= n
          then None
          else
            match tokens.(i) with
            | Backtick ->
                let buf = Buffer.create 16 in
                for j = pos + 1 to i - 1 do
                  Buffer.add_string buf (Lexer.token_to_literal tokens.(j))
                done;
                Some (CodeInlineNode (Buffer.contents buf), i + 1 - pos)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 1)
    | _ -> None

let render_html _reg id = function
  | CodeInlineNode content ->
      Some (Printf.sprintf "<code%s>%s</code>" id content)
  | _ -> None

let render_tex _reg _id = function
  | CodeInlineNode content -> Some ("\\lstinline|" ^ content ^ "|")
  | _ -> None
