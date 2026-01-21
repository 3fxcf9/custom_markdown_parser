open Ast
open Lexer

(* gather tokens for a single line (EXCLUDING trailing Newline if present) *)
let gather_line_no_newline (tokens : Lexer.token array) (pos : int) :
    Lexer.token array * int =
  let n = Array.length tokens in
  let rec loop i = if i >= n || tokens.(i) = Newline then i else loop (i + 1) in
  let i = loop pos in
  (Array.sub tokens pos (i - pos), i - pos)

let parse_block (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  let n = Array.length tokens in
  if pos + 2 >= n then None
  else
    match (tokens.(pos), tokens.(pos + 1), tokens.(pos + 2)) with
    | Backtick, Backtick, Backtick ->
        let line, consumed = gather_line_no_newline tokens (pos + 3) in
        let lang =
          if Array.length line < 1 then None
          else match line.(0) with Text s -> Some s | _ -> None
        in
        let rec find_close i =
          if i + 2 >= n then None
          else
            match (tokens.(i), tokens.(i + 1), tokens.(i + 2)) with
            | Backtick, Backtick, Backtick ->
                let buf = Buffer.create 32 in
                for j = pos + 3 + consumed to i - 1 do
                  Buffer.add_string buf (Lexer.token_to_literal tokens.(j))
                done;
                let content = Buffer.contents buf |> String.trim in
                Some (CodeBlockNode (lang, content), i + 3 - pos + consumed)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 2 + consumed)
    | _ -> None

let parse_inline _ _ _ _ = None

let render_html _reg id = function
  | CodeBlockNode (Some lang, content) ->
      Some
        (Printf.sprintf "<pre><code%s class=\"language-%s\">%s</code></div>" id
           lang content)
  | CodeBlockNode (None, content) ->
      Some (Printf.sprintf "<pre><code%s>%s</code></div>" id content)
  | _ -> None

let render_tex _reg _id = function
  | CodeBlockNode (_, content) ->
      Some ("\\begin{lstlisting}" ^ content ^ "\\end{lstlisting}")
  | _ -> None
