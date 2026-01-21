open Lexer
open Ast

let parse_block _ _ _ = None

let parse_url tokens pos =
  match tokens.(pos) with
  | Lparen ->
      let rec find_close i count_math count_code count_parenthesis
          count_brackets =
        if i >= Array.length tokens then None
        else
          match tokens.(i) with
          | Backtick ->
              find_close (i + 1) count_math (count_code + 1) count_parenthesis
                count_brackets
          | Dollar ->
              find_close (i + 1) (count_math + 1) count_code count_parenthesis
                count_brackets
          | Lbracket ->
              find_close (i + 1) count_math count_code count_parenthesis
                (count_brackets + 1)
          | Rbracket ->
              find_close (i + 1) count_math count_code count_parenthesis
                (count_brackets + 1)
          | Lparen ->
              find_close (i + 1) count_math count_code (count_parenthesis + 1)
                count_brackets
          | Rparen
            when count_math mod 2 = 0
                 && count_code mod 2 = 0
                 && count_brackets mod 2 = 0
                 && count_parenthesis mod 2 = 0 ->
              let inner_link = Array.sub tokens (pos + 1) (i - pos - 1) in
              let url =
                Array.fold_left
                  (fun acc t -> acc ^ Lexer.token_to_literal t)
                  "" inner_link
                |> String.trim
              in
              Some (url, i)
          | Rparen ->
              find_close (i + 1) count_math count_code (count_parenthesis + 1)
                count_brackets
          | _ ->
              find_close (i + 1) count_math count_code count_parenthesis
                count_brackets
      in
      find_close (pos + 1) 0 0 0 0
  | _ -> None

let parse_inline (tokens : Lexer.token array) (pos : int) reg =
  if pos + 3 >= Array.length tokens then None
  else
    match tokens.(pos) with
    | Lbracket ->
        let rec find_close i count_math count_code count_parenthesis
            count_brackets =
          if i >= Array.length tokens then None
          else
            match tokens.(i) with
            | Backtick ->
                find_close (i + 1) count_math (count_code + 1) count_parenthesis
                  count_brackets
            | Dollar ->
                find_close (i + 1) (count_math + 1) count_code count_parenthesis
                  count_brackets
            | Lparen ->
                find_close (i + 1) count_math count_code (count_parenthesis + 1)
                  count_brackets
            | Rparen ->
                find_close (i + 1) count_math count_code (count_parenthesis + 1)
                  count_brackets
            | Lbracket ->
                find_close (i + 1) count_math count_code count_parenthesis
                  (count_brackets + 1)
            | Rbracket
              when count_math mod 2 = 0
                   && count_code mod 2 = 0
                   && count_brackets mod 2 = 0
                   && count_parenthesis mod 2 = 0 -> (
                let inner = Array.sub tokens (pos + 1) (i - pos - 1) in
                let content = Parser.parse_inlines reg inner in
                match parse_url tokens (i + 1) with
                | Some (url, end_index) ->
                    Some (LinkNode (content, url), end_index - pos + 1)
                | None -> None)
            | Rbracket ->
                find_close (i + 1) count_math count_code count_parenthesis
                  (count_brackets - 1)
            | _ ->
                find_close (i + 1) count_math count_code count_parenthesis
                  count_brackets
        in
        find_close (pos + 1) 0 0 0 0
    | _ -> None

let render_html reg id = function
  | LinkNode (children, link) ->
      Some
        (Printf.sprintf "<a%s href=\"%s\">%s</a>" id link
           (String.concat ""
              (List.map (Registry.render_html reg None) children)))
  | _ -> None

let render_tex _ _ _ = failwith "not handled"
