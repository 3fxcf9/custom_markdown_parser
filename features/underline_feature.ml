open Lexer
open Ast

let parse_block _ _ _ = None

let parse_inline (tokens : Lexer.token array) (pos : int) reg =
  if pos + 1 >= Array.length tokens then None
  else
    match (tokens.(pos), tokens.(pos + 1)) with
    | Plus, Plus ->
        let rec find_close i count_math count_code =
          if i + 1 >= Array.length tokens then None
          else
            match (tokens.(i), tokens.(i + 1)) with
            | Backtick, _ -> find_close (i + 1) count_math (count_code + 1)
            | Dollar, _ -> find_close (i + 1) (count_math + 1) count_code
            | Plus, Plus when count_math mod 2 = 0 && count_code mod 2 = 0 ->
                let inner = Array.sub tokens (pos + 2) (i - pos - 2) in
                let content = Parser.parse_inlines reg inner in
                Some (UnderlineNode content, i + 2 - pos)
            | _ -> find_close (i + 1) count_math count_code
        in
        find_close (pos + 2) 0 0
    | _ -> None

let render_html reg id = function
  | UnderlineNode children ->
      Some
        (Printf.sprintf "<ul%s>%s</ul>" id
           (String.concat ""
              (List.map (Registry.render_html reg None) children)))
  | _ -> None

let render_tex reg _id = function
  | UnderlineNode children ->
      Some
        ("\\uline{"
        ^ String.concat "" (List.map (Registry.render_tex reg) children)
        ^ "}")
  | _ -> None
