open Ast
open Lexer

let parse_block (tokens : Lexer.token array) (pos : int) _reg =
  let n = Array.length tokens in
  if pos + 1 >= n then None
  else
    match (tokens.(pos), tokens.(pos + 1)) with
    | Dollar, Dollar ->
        let rec find_close i =
          if i + 1 >= n then None
          else
            match (tokens.(i), tokens.(i + 1)) with
            | Dollar, Dollar ->
                let buf = Buffer.create 32 in
                for j = pos + 2 to i - 1 do
                  Buffer.add_string buf (Lexer.token_to_literal tokens.(j))
                done;
                let content = Buffer.contents buf |> String.trim in
                Some (MathDisplayNode content, i + 2 - pos)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 2)
    | _ -> None

let parse_inline _ _ _ = None

let render_html _reg id = function
  | MathDisplayNode content ->
      Some
        (Printf.sprintf "<code class=\"math-display\"%s>%s</code>" id content)
  | _ -> None

let render_tex _reg _id = function
  | MathDisplayNode content -> Some ("\n\\[\n" ^ content ^ "\n\\]")
  | _ -> None
