open Lexer
open Ast

let parse_block _ _ _ _ = None

let parse_inline (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) reg =
  if pos >= Array.length tokens then None
  else
    match tokens.(pos) with
    | (Star | Underscore) as open_kind ->
        let rec find_close i count_math count_code count_bracket =
          if i >= Array.length tokens then None
          else
            match tokens.(i) with
            | Backtick ->
                find_close (i + 1) count_math (count_code + 1) count_bracket
            | Dollar ->
                find_close (i + 1) (count_math + 1) count_code count_bracket
            | Lbracket | Rbracket ->
                find_close (i + 1) count_math count_code (count_bracket + 1)
            | k
              when k = open_kind
                   && count_math mod 2 = 0
                   && count_code mod 2 = 0
                   && count_bracket mod 2 = 0 ->
                let inner = Array.sub tokens (pos + 1) (i - pos - 1) in
                let content = Parser.parse_inlines reg inner in
                Some (ItalicNode content, i - pos + 1)
            | _ -> find_close (i + 1) count_math count_code count_bracket
        in
        find_close (pos + 1) 0 0 0
    | _ -> None

let render_html reg id = function
  | ItalicNode children ->
      Some
        (Printf.sprintf "<em%s>%s</em>" id
           (String.concat ""
              (List.map (Registry.render_html reg None) children)))
  | _ -> None

let render_tex reg _id = function
  | ItalicNode children ->
      Some
        ("\\textt{"
        ^ String.concat "" (List.map (Registry.render_tex reg) children)
        ^ "}")
  | _ -> None
