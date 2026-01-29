open Ast
open Lexer

(* Tilde is for compact list (each item is parsed inline) *)

(* gather tokens for a single line (including trailing Newline if present) *)
let gather_line (tokens : Lexer.token array) (pos : int) :
    Lexer.token array * int =
  let n = Array.length tokens in
  let rec loop i = if i >= n || tokens.(i) = Newline then i else loop (i + 1) in
  let i = loop pos in
  if i >= n
  then (Array.sub tokens pos (i - pos), i - pos)
  else (Array.sub tokens pos (i - pos + 1), i - pos + 1)

let is_digits s =
  try
    ignore (int_of_string s);
    true
  with _ -> false

let token_is_space = function Space -> true | _ -> false

let is_list_item (tokens : Lexer.token array) (pos : int)
    (after_reference : bool) : bool =
  (* must be at start of a line *)
  if pos > 0 && tokens.(pos - 1) <> Newline && not after_reference
  then false
  else
    let n = Array.length tokens in
    if pos + 1 >= n
    then false
    else
      match (tokens.(pos), tokens.(pos + 1)) with
      | Dash, Space | Star, Space | Plus, Space | Tilde, Space -> true
      | Text s, Dot when pos + 2 < n && tokens.(pos + 2) = Space -> is_digits s
      | _ -> false

let is_compatible_list_item (tokens : Lexer.token array) (pos : int)
    (open_tok : Lexer.token) (next_numbered_index : int) : bool =
  let n = Array.length tokens in
  if pos >= n
  then false
  else
    match (open_tok, tokens.(pos)) with
    | Text _, Text s ->
        pos + 2 < n
        && tokens.(pos + 1) = Dot
        && tokens.(pos + 2) = Space
        && is_digits s
        && int_of_string s = next_numbered_index
    | Dash, Dash | Star, Star | Plus, Plus | Tilde, Tilde ->
        pos + 1 < n && tokens.(pos + 1) = Space
    | _ -> false

let min_indent_for_marker = function
  | Text s -> String.length s + 2 (* number + '.' + ' ' *)
  | _ -> 2

(* Append an array slice [start..start+len-1] to a rev-list of tokens *)
let append_array_slice_to_rev_list arr start len acc_rev =
  let rec loop j acc =
    if j >= start + len then acc else loop (j + 1) (arr.(j) :: acc)
  in
  loop start acc_rev

let rec collect_indented_lines tokens pos indent_min acc_rev =
  let n = Array.length tokens in
  if pos >= n
  then (acc_rev, pos)
  else
    match tokens.(pos) with
    | Indent lvl when lvl >= indent_min ->
        let line_arr, consumed = gather_line tokens pos in
        if Array.length line_arr = 0
        then collect_indented_lines tokens (pos + consumed) indent_min acc_rev
        else
          (* adjust leading indent token if present *)
          let adjusted_list =
            match line_arr.(0) with
            | Indent lvl when lvl >= indent_min ->
                let new_lvl = lvl - indent_min in
                if new_lvl > 0
                then (
                  let a = Array.copy line_arr in
                  a.(0) <- Indent new_lvl;
                  Array.to_list a)
                else
                  (* drop the indent token *)
                  Array.to_list
                    (Array.sub line_arr 1 (Array.length line_arr - 1))
            | _ -> Array.to_list line_arr
          in
          let acc_rev' =
            List.fold_left (fun acc t -> t :: acc) acc_rev adjusted_list
          in
          collect_indented_lines tokens (pos + consumed) indent_min acc_rev'
    | Newline -> collect_indented_lines tokens (pos + 1) indent_min acc_rev
    | _ ->
        (* Not an indented continuation -> stop collecting *)
        (acc_rev, pos)

let parse_single_item tokens pos open_tok reg =
  let first_line_arr, first_consumed = gather_line tokens pos in
  let prefix_len = match open_tok with Text _ -> 3 | _ -> 2 in
  let first_len = Array.length first_line_arr in
  (* acc_rev: list containing all the tokens *inside* the list item in reverse order (with updated indent) *)
  let acc_rev =
    if prefix_len < first_len
    then
      append_array_slice_to_rev_list first_line_arr prefix_len
        (first_len - prefix_len) []
    else []
  in
  let indent_min = min_indent_for_marker open_tok in
  let acc_rev', pos_after =
    collect_indented_lines tokens (pos + first_consumed) indent_min acc_rev
  in
  let item_tokens = List.rev acc_rev' |> Array.of_list in
  let parsed =
    if Array.length item_tokens = 0
    then []
    else
      match open_tok with
      | Tilde -> Parser.parse_inlines reg item_tokens
      | _ -> Parser.parse reg item_tokens
  in
  (parsed, pos_after)

(* parse multiple consecutive list items that share the same marker (open_tok) starting at pos.
   returns (items_parsed_list, pos_after_items)
*)
let rec parse_items tokens pos open_tok next_numbered_index reg acc =
  if not (is_compatible_list_item tokens pos open_tok next_numbered_index)
  then (List.rev acc, pos)
  else
    let item_parsed, next_pos = parse_single_item tokens pos open_tok reg in
    parse_items tokens next_pos open_tok (next_numbered_index + 1) reg
      (item_parsed :: acc)

let parse_inline _ _ _ _ = None

let parse_block (tokens : Lexer.token array) (pos : int)
    (after_reference : bool) reg =
  if not (is_list_item tokens pos after_reference)
  then None
  else
    let open_tok = tokens.(pos) in
    let ltype =
      match open_tok with
      | Text _ -> Ordered
      | Dash -> Dash
      | Star -> Star
      | Plus -> Plus
      | Tilde -> Compact
      | _ -> Dash
    in
    let start_num =
      match open_tok with
      | Text s -> ( try int_of_string s with _ -> 1)
      | _ -> 1
    in
    let items, pos_after = parse_items tokens pos open_tok start_num reg [] in
    let consumed = pos_after - pos in
    Some (ListNode (ltype, start_num, items), consumed)

let render_html reg id = function
  | ListNode (ltype, start_num, items) ->
      let tag = match ltype with Ordered -> "ol" | _ -> "ul" in
      let args =
        match ltype with
        | Ordered when start_num <> 1 ->
            Printf.sprintf " start=\"%d\"" start_num
        | Dash -> " class=\"list-dash\""
        | Star -> " class=\"list-star\""
        | Plus -> " class=\"list-plus\""
        | Compact -> " class=\"list-compact\""
        | _ -> ""
      in
      let buf = Buffer.create 128 in
      Buffer.add_string buf (Printf.sprintf "<%s%s%s>\n" tag args id);
      List.iter
        (fun item_children ->
          Buffer.add_string buf "<li>";
          List.iter
            (fun child ->
              Buffer.add_string buf (Registry.render_html reg None child))
            item_children;
          Buffer.add_string buf "</li>\n")
        items;
      Buffer.add_string buf (Printf.sprintf "</%s>" tag);
      Some (Buffer.contents buf)
  | _ -> None

let render_tex reg _id = function
  | ListNode (ltype, _start, items) ->
      let env = match ltype with Ordered -> "enumerate" | _ -> "itemize" in
      let buf = Buffer.create 256 in
      Buffer.add_string buf (Printf.sprintf "\n\\\\begin{%s}\n" env);
      List.iter
        (fun item_children ->
          Buffer.add_string buf "  \\\\item ";
          List.iteri
            (fun _ child ->
              let rendered = Registry.render_tex reg child in
              let lines = String.split_on_char '\n' rendered in
              List.iteri
                (fun li line ->
                  if li = 0
                  then Buffer.add_string buf (line ^ "\n")
                  else Buffer.add_string buf ("        " ^ line ^ "\n"))
                lines)
            item_children;
          Buffer.add_string buf "\n")
        items;
      Buffer.add_string buf (Printf.sprintf "\\\\end{%s}\n" env);
      Some (Buffer.contents buf)
  | _ -> None
