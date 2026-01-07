open Registry
open Lexer
open Ast

(* Try parsers sequentially; returns Some (node, consumed) or None *)
let rec try_parsers parsers tokens pos reg =
  match parsers with
  | [] -> None
  | p :: tl -> (
      match p tokens pos reg with
      | Some _ as res -> res
      | None -> try_parsers tl tokens pos reg)

let is_blank_tokens (tokens : Lexer.token array) : bool =
  Array.for_all
    (function
      | Newline | Space -> true
      | Indent n -> n = 0
      | Text s -> String.trim s = ""
      | _ -> false)
    tokens

let parse_inlines (reg : Registry.t) (tokens : Lexer.token array) :
    Ast.node list =
  let acc = ref [] in
  let i = ref 0 in
  let n = Array.length tokens in
  while !i < n do
    match try_parsers reg.inline_parsers tokens !i reg with
    | Some (node, consumed) ->
        acc := node :: !acc;
        i := !i + consumed
    | None ->
        (match tokens.(!i) with
        | Newline -> ()
        | t ->
            let node = TextNode (Lexer.token_to_literal t) in
            acc := node :: !acc);
        i := !i + 1
  done;
  List.rev !acc

let parse (reg : Registry.t) (tokens : Lexer.token array) : Ast.node list =
  let acc = ref [] in
  let i = ref 0 in
  let n = Array.length tokens in

  while !i < n do
    match try_parsers reg.block_parsers tokens !i reg with
    | Some (node, consumed) ->
        acc := node :: !acc;
        i := !i + consumed
    | None ->
        (* fallback: collect tokens for a paragraph until a stop_condition is reached *)
        let start = !i in
        let rec advance () =
          if !i >= n then ()
          else
            (* stop on a single newline or on a stop_condition *)
            match tokens.(!i) with
            | Newline -> i := !i + 1
            | _ ->
                if
                  List.exists
                    (fun cond -> cond tokens !i reg)
                    reg.paragraph_stop_conditions
                then ()
                else (
                  i := !i + 1;
                  advance ())
        in
        advance ();
        if start <> !i then
          let len = !i - start in
          if len > 0 then
            let slice = Array.sub tokens start len in
            if not (is_blank_tokens slice) then (
              let inline_nodes = parse_inlines reg slice in
              acc := ParagraphNode inline_nodes :: !acc;
              (* skip newline (line skip handled above) *)
              if !i < n then
                match tokens.(!i) with Newline -> i := !i + 1 | _ -> ())
  done;
  List.rev !acc
