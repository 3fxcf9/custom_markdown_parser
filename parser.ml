(** Block and inline parsing logic. *)

open Registry
open Lexer
open Ast

(* Try parsers sequentially; returns Some (node, consumed) or None *)
let rec try_parsers parsers tokens pos after_reference reg =
  match parsers with
  | [] -> None
  | p :: tl -> (
      match p tokens pos after_reference reg with
      | Some _ as res -> res
      | None -> try_parsers tl tokens pos after_reference reg)

let is_blank_tokens (tokens : Lexer.token array) : bool =
  Array.for_all
    (function
      | Newline | Space -> true
      | Indent n -> n = 0
      | Text s -> String.trim s = ""
      | _ -> false)
    tokens

let get_tag node reg =
  let heading_tag = Heading_labels.heading_of_array reg.heading_position in
  match node with
  | HeadingNode (_l, _) -> Some heading_tag
  | EnvironmentNode (name, _, _) ->
      let environment_count =
        match Hashtbl.find_opt reg.environment_count name with
        | Some n -> n
        | None -> 0
      in
      Some
        (Printf.sprintf "%s %s.%d"
           (environment_display_name name)
           heading_tag environment_count)
  | _ -> None

(** [update_position node reg] Increments internal counters for headings and
    environments. *)
let update_position node reg =
  match node with
  | HeadingNode (l, _) ->
      Hashtbl.clear reg.environment_count;
      reg.heading_position.(l) <- reg.heading_position.(l) + 1;
      if l < 6 then Array.fill reg.heading_position (l + 1) (6 - l) 0
  | EnvironmentNode (name, _, _) ->
      let new_count =
        match Hashtbl.find_opt reg.environment_count name with
        | Some n -> n + 1
        | None -> 1
      in
      Hashtbl.replace reg.environment_count name new_count
  | _ -> ()

(** [parse_inlines reg tokens] Parses a slice of tokens specifically for inline
    elements (bold, link, etc.). *)
let parse_inlines (reg : Registry.t) (tokens : Lexer.token array) :
    Ast.node list =
  let acc = ref [] in
  let i = ref 0 in
  let n = Array.length tokens in
  let after_reference = ref false in
  while !i < n do
    match try_parsers reg.inline_parsers tokens !i !after_reference reg with
    | Some (node, consumed) ->
        (after_reference :=
           match node with ReferenceTagNode _ -> true | _ -> false);
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

(** [parse reg tokens] Parses an array of tokens into a block-level AST. *)
let parse (reg : Registry.t) (tokens : Lexer.token array) : Ast.node list =
  let acc = ref [] in
  let i = ref 0 in
  let n = Array.length tokens in
  let after_reference = ref false in
  let reference_id = ref "" in

  while !i < n do
    match try_parsers reg.block_parsers tokens !i !after_reference reg with
    | Some (node, consumed) ->
        (if !after_reference
         then
           match get_tag node reg with
           | Some s -> Hashtbl.replace reg.references !reference_id s
           | None -> ());
        (after_reference :=
           match node with
           | ReferenceTagNode r ->
               reference_id := r;
               true
           | _ -> false);
        update_position node reg;
        acc := node :: !acc;
        i := !i + consumed
    | None ->
        (* fallback: collect tokens for a paragraph until a stop_condition is reached *)
        let start = !i in
        let rec advance () =
          if !i >= n
          then ()
          else
            (* stop on a single newline or on a stop_condition *)
            match tokens.(!i) with
            | Newline -> i := !i + 1
            | _ ->
                if
                  List.exists
                    (fun cond -> cond tokens !i !after_reference reg)
                    reg.paragraph_stop_conditions
                then ()
                else (
                  i := !i + 1;
                  advance ())
        in
        advance ();
        if start <> !i
        then
          let len = !i - start in
          if len > 0
          then
            let slice = Array.sub tokens start len in
            if not (is_blank_tokens slice)
            then (
              let inline_nodes = parse_inlines reg slice in
              acc := ParagraphNode inline_nodes :: !acc;
              (* skip newline (line skip handled above) *)
              if !i < n
              then match tokens.(!i) with Newline -> i := !i + 1 | _ -> ())
  done;
  List.rev !acc
