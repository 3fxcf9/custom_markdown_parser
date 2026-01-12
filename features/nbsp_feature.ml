open Ast
open Lexer

type node += NbspNode of bool

let parse_block _ _ _ = None

let parse_inline tokens pos _reg =
  let n = Array.length tokens in
  if pos + 1 >= n then None
  else
    match tokens.(pos) with
    | Tilde -> Some (NbspNode (pos + 1 < n - 1 && tokens.(pos + 1) = Colon), 1)
    | _ -> None

let render_html _reg = function
  | NbspNode true -> Some "&#8239;"
  | NbspNode false -> Some "&nbsp;"
  | _ -> None

let render_tex _reg = function
  | NbspNode true -> Some "\\thinspace"
  | NbspNode false -> Some "\\nobreakspace;"
  | _ -> None
