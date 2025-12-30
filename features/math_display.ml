open Ast
open Lexer

type node += MathDisplayNode of string

(* scan $$ ... $$ starting at position
   returns (content, consumed_tokens) *)
let scan_math_block tokens pos =
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
                Some (content, i + 2 - pos)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 2)
    | _ -> None

let paragraph_stop_condition tokens pos =
  match scan_math_block tokens pos with Some _ -> true | None -> false

let parse_block tokens pos _reg =
  match scan_math_block tokens pos with
  | Some (content, consumed) -> Some (MathDisplayNode content, consumed)
  | None -> None

let parse_inline _ _ _ = None

let render_html _reg = function
  | MathDisplayNode content ->
      Some ("<code class=\"math-display\">" ^ content ^ "</code>")
  | _ -> None

let render_tex _reg = function
  | MathDisplayNode content -> Some ("\n\\[\n" ^ content ^ "\n\\]")
  | _ -> None
