open Lexer
open Ast

let parse_block _ _ _ _ = None

let parse_inline (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  if pos + 1 >= Array.length tokens then None
  else
    match (tokens.(pos), tokens.(pos + 1)) with
    | Hash, Lbracket ->
        let rec find_close i =
          if i >= Array.length tokens then None
          else
            match tokens.(i) with
            | Rbracket ->
                let inner = Array.sub tokens (pos + 2) (i - pos - 2) in
                let content =
                  Array.fold_left
                    (fun acc t ->
                      acc
                      ^
                      match Lexer.token_to_literal t with " " -> "-" | e -> e)
                    "" inner
                in
                Some (ReferenceNode content, i - pos + 2)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 2)
    | _ -> None

let render_html (reg : Registry.t) _id = function
  | ReferenceNode id ->
      let label =
        match Hashtbl.find_opt reg.references id with Some l -> l | None -> id
      in
      Some (Printf.sprintf "<a href=\"%s\">%s</a>" id label)
  | _ -> None

let render_tex _ _ _ = failwith "not implemented"
