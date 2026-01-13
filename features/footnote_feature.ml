open Lexer

type Ast.node += FootnoteNode of Ast.node list

let parse_block _ _ _ = None

let parse_inline tokens pos registry =
  if pos + 1 >= Array.length tokens then None
  else
    match (tokens.(pos), tokens.(pos + 1)) with
    | Lparen, Lparen ->
        let rec find_close i count_math count_code count_paren =
          if i + 1 >= Array.length tokens then None
          else
            match (tokens.(i), tokens.(i + 1)) with
            | Backtick, _ ->
                find_close (i + 1) count_math (count_code + 1) count_paren
            | Dollar, _ ->
                find_close (i + 1) (count_math + 1) count_code count_paren
            | Rparen, Rparen
              when count_math mod 2 = 0
                   && count_code mod 2 = 0
                   && count_paren mod 2 = 0 ->
                let inner = Array.sub tokens (pos + 2) (i - pos - 2) in
                let content = Parser.parse_inlines registry inner in
                Some (FootnoteNode content, i + 2 - pos)
            | Lparen, _ | Rparen, _ ->
                find_close (i + 1) count_math count_code (count_paren + 1)
            | _ -> find_close (i + 1) count_math count_code count_paren
        in
        find_close (pos + 2) 0 0 0
    | _ -> None

let render_html reg = function
  | FootnoteNode children ->
      Some
        ("<span class=\"sidenote-number\"><small class=\"sidenote\">"
        ^ String.concat "" (List.map (Registry.render_html reg) children)
        ^ "</small></span>")
  | _ -> None

let render_tex _ _ = failwith "not handled"
