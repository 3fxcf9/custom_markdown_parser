open Ast
open Lexer

let parse_block _ _ _ _ = None

let parse_inline (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  let n = Array.length tokens in
  if pos + 1 >= n then None
  else
    match tokens.(pos) with
    | Tilde -> Some (NbspNode (pos + 1 < n - 1 && tokens.(pos + 1) = Colon), 1)
    | _ -> None

let render_html _reg _id = function
  | NbspNode true -> Some "&#8239;"
  | NbspNode false -> Some "&nbsp;"
  | _ -> None

let render_tex _reg _id = function
  | NbspNode true -> Some "\\thinspace"
  | NbspNode false -> Some "\\nobreakspace;"
  | _ -> None
