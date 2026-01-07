open Ast
open Lexer

type hr_kind = Compact | Solid | Dashed | Sawteeth
type node += HRuleNode of hr_kind

let hr_kind_of_token = function
  | Tilde -> Some Compact
  | Equal -> Some Solid
  | Dash -> Some Dashed
  | Caret -> Some Sawteeth
  | _ -> None

let min_count = function Compact -> 1 | Solid | Dashed | Sawteeth -> 3

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

let parse_block tokens pos _reg =
  match is_hrule_line tokens pos with
  | Some (kind, consumed) -> Some (HRuleNode kind, consumed)
  | None -> None

let parse_inline _ _ _ = None

let render_html _reg = function
  | HRuleNode Compact -> Some "<hr class=\"style-compact\">"
  | HRuleNode Solid -> Some "<hr class=\"style-solid\">"
  | HRuleNode Dashed -> Some "<hr class=\"style-dashed\">"
  | HRuleNode Sawteeth -> Some "<hr class=\"style-sawteeth\">"
  | _ -> None

let render_tex _reg = function
  | HRuleNode _ -> Some "\\par\\noindent\\rule{\\textwidth}{0.4pt}\\par"
  | _ -> None
