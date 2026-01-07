open Ast
open Lexer

type node += MathInlineNode of string

let parse_block _ _ _ = None

let parse_inline tokens pos _reg =
  let n = Array.length tokens in
  if pos + 1 >= n then None
  else
    match tokens.(pos) with
    | Dollar ->
        let rec find_close i =
          if i >= n then None
          else
            match tokens.(i) with
            | Dollar ->
                let buf = Buffer.create 16 in
                for j = pos + 1 to i - 1 do
                  Buffer.add_string buf (Lexer.token_to_literal tokens.(j))
                done;
                Some (MathInlineNode (Buffer.contents buf), i + 1 - pos)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 1)
    | _ -> None

let render_html _reg = function
  | MathInlineNode content ->
      Some ("<code class=\"math-inline\">" ^ content ^ "</code>")
  | _ -> None

let render_tex _reg = function
  | MathInlineNode content -> Some ("$" ^ content ^ "$")
  | _ -> None
