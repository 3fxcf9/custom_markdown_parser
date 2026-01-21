open Ast
open Lexer

let parse_inline _ _ _ = None

let rec count_hash tokens pos =
  if pos >= Array.length tokens then 0
  else
    match tokens.(pos) with Hash -> 1 + count_hash tokens (pos + 1) | _ -> 0

(* gather tokens for a single line (including trailing Newline if present) *)
let gather_line (tokens : Lexer.token array) (pos : int) :
    Lexer.token array * int =
  let n = Array.length tokens in
  let rec loop i = if i >= n || tokens.(i) = Newline then i else loop (i + 1) in
  let i = loop pos in
  if i >= n then (Array.sub tokens pos (i - pos), i - pos)
  else (Array.sub tokens pos (i - pos + 1), i - pos + 1)

let parse_block (tokens : Lexer.token array) (pos : int) reg =
  if pos > 0 && tokens.(pos - 1) <> Newline then None
  else
    match count_hash tokens pos with
    | l when l >= 1 && l <= 6 -> (
        let n = Array.length tokens in
        if pos + l >= n then None
        else
          match tokens.(pos + l) with
          | Space ->
              (* +1: skip space *)
              let line_tokens, consumed = gather_line tokens (pos + l + 1) in
              let content = Parser.parse_inlines reg line_tokens in
              Some (HeadingNode (l, content), l + 1 + consumed)
          | _ -> None)
    | _ -> None

let render_html reg id = function
  | HeadingNode (level, children) ->
      let html =
        String.concat "" (List.map (Registry.render_html reg None) children)
      in
      Some (Printf.sprintf "<h%d%s>%s</h%d>" level id html level)
  | _ -> None

let render_tex _ _ = failwith "not handled"
