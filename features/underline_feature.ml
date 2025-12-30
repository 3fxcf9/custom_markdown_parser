open Lexer

type Ast.node += UnderlineNode of Ast.node list

let paragraph_stop_condition _ _ = false
let parse_block _ _ _ = None

let parse_inline tokens pos registry =
  if pos + 1 >= Array.length tokens then None
  else
    match (tokens.(pos), tokens.(pos + 1)) with
    | Plus, Plus ->
        let rec find_close i =
          if i + 1 >= Array.length tokens then None
          else
            match (tokens.(i), tokens.(i + 1)) with
            | Plus, Plus ->
                let inner = Array.sub tokens (pos + 2) (i - pos - 2) in
                let content = Parser.parse_inlines registry inner in
                Some (UnderlineNode content, i + 2 - pos)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 2)
    | _ -> None

let render_html reg = function
  | UnderlineNode children ->
      Some
        ("<u>"
        ^ String.concat "" (List.map (Registry.render_html reg) children)
        ^ "</u>")
  | _ -> None

let render_tex reg = function
  | UnderlineNode children ->
      Some
        ("\\uline{"
        ^ String.concat "" (List.map (Registry.render_tex reg) children)
        ^ "}")
  | _ -> None
