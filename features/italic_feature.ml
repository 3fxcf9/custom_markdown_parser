open Lexer

type Ast.node += ItalicNode of Ast.node list

let parse_block _ _ _ = None

let parse_inline tokens pos registry =
  if pos >= Array.length tokens then None
  else
    match tokens.(pos) with
    | (Star | Underscore) as open_kind ->
        let rec find_close i count_math count_code =
          if i >= Array.length tokens then None
          else
            match tokens.(i) with
            | Backtick -> find_close (i + 1) count_math (count_code + 1)
            | Dollar -> find_close (i + 1) (count_math + 1) count_code
            | k
              when k = open_kind && count_math mod 2 = 0 && count_code mod 2 = 0
              ->
                let inner = Array.sub tokens (pos + 1) (i - pos - 1) in
                let content = Parser.parse_inlines registry inner in
                Some (ItalicNode content, i - pos + 1)
            | _ -> find_close (i + 1) count_math count_code
        in
        find_close (pos + 1) 0 0
    | _ -> None

let render_html reg = function
  | ItalicNode children ->
      Some
        ("<em>"
        ^ String.concat "" (List.map (Registry.render_html reg) children)
        ^ "</em>")
  | _ -> None

let render_tex reg = function
  | ItalicNode children ->
      Some
        ("\\textt{"
        ^ String.concat "" (List.map (Registry.render_tex reg) children)
        ^ "}")
  | _ -> None
