open Lexer
open Ast

let parse_block (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) _reg =
  if pos + 1 >= Array.length tokens
  then None
  else
    match (tokens.(pos), tokens.(pos + 1)) with
    | At, Lbracket ->
        let rec find_close i =
          if i >= Array.length tokens
          then None
          else
            match tokens.(i) with
            | Rbracket ->
                let inner = Array.sub tokens (pos + 2) (i - pos - 2) in
                let content =
                  Array.fold_left
                    (fun acc t ->
                      acc
                      ^
                      match Lexer.token_to_literal t with
                      | " " -> "-"
                      | e -> e)
                    "" inner
                in
                Some (ImageNode content, i - pos + 2)
            | _ -> find_close (i + 1)
        in
        find_close (pos + 2)
    | _ -> None

let parse_inline = parse_block

let render_html (reg : Registry.t) _id = function
  | ImageNode src -> (
      match Hashtbl.find_opt reg.figures (Filename.basename src) with
      | Some code -> Some code
      | None -> Some (Printf.sprintf "<img src=\"%s\"/>" src))
  | _ -> None

let render_tex _ _ _ = failwith "not implemented"
