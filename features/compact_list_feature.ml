open Lexer
open Ast

(* Requires list_feature to be enabled *)

let parse_inline _ _ _ _ = None

let parse_block (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) reg =
  let n = Array.length tokens in
  if pos + 1 >= n
  then None
  else if tokens.(pos) <> Tilde || tokens.(pos + 1) <> Space
  then None
  else
    (* collect tokens until newline *)
    let rec collect i acc =
      if i >= n || tokens.(i) = Newline
      then (List.rev acc, i)
      else collect (i + 1) (tokens.(i) :: acc)
    in
    let content_tokens, stop_pos = collect (pos + 2) [] in
    let arr = Array.of_list content_tokens in

    (* split on "  ~ " *)
    let rec split acc current i =
      if i >= Array.length arr
      then List.rev (Array.of_list (List.rev current) :: acc)
      else if
        i + 3 < Array.length arr
        && arr.(i) = Space
        && arr.(i + 1) = Space
        && arr.(i + 2) = Tilde
        && arr.(i + 3) = Space
      then split (Array.of_list (List.rev current) :: acc) [] (i + 4)
      else split acc (arr.(i) :: current) (i + 1)
    in

    let items_tokens = split [] [] 0 in

    (* Only parse two or more items *)
    match items_tokens with
    | _ :: _ :: _ ->
        let items =
          List.map
            (fun tok_arr -> Parser.parse_inlines reg tok_arr)
            items_tokens
        in
        Some (ListNode (Compact, 1, items), stop_pos - pos)
    | _ -> None

let render_html _ _ _ = None
let render_tex _ _ _ = None
