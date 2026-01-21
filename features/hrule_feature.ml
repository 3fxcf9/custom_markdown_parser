open Ast
open Lexer

let hr_kind_of_token token : hr_kind option =
  match token with
  | Tilde -> Some Compact
  | Equal -> Some Solid
  | Dash -> Some Dashed
  | Caret -> Some Sawteeth
  | _ -> None

let min_count (hr_type : hr_kind) =
  match hr_type with Compact -> 1 | Solid | Dashed | Sawteeth -> 3

(* Number of tokens to consume INCLUDING newline *)
let is_hrule_line tokens pos =
  let n = Array.length tokens in
  if pos >= n then None
  else
    match hr_kind_of_token tokens.(pos) with
    | None -> None
    | Some kind ->
        let i = ref pos in
        while !i < n && tokens.(!i) = tokens.(pos) do
          incr i
        done;
        let count = !i - pos in
        if count < min_count kind then None
        else if !i < n && tokens.(!i) = Newline then Some (kind, count + 1)
        else None

let parse_block (tokens : Lexer.token array) (pos : int) _reg =
  match is_hrule_line tokens pos with
  | Some (kind, consumed) -> Some (HRuleNode kind, consumed)
  | None -> None

let parse_inline _ _ _ = None

let render_html _reg id = function
  | HRuleNode Compact ->
      Some (Printf.sprintf "<hr%s class=\"style-compact\">" id)
  | HRuleNode Solid -> Some (Printf.sprintf "<hr%s class=\"style-solid\">" id)
  | HRuleNode Dashed -> Some (Printf.sprintf "<hr%s class=\"style-dashed\">" id)
  | HRuleNode Sawteeth ->
      Some (Printf.sprintf "<hr%s class=\"style-sawteeth\">" id)
  | _ -> None

let render_tex _reg _id = function
  | HRuleNode _ -> Some "\\par\\noindent\\rule{\\textwidth}{0.4pt}\\par"
  | _ -> None
