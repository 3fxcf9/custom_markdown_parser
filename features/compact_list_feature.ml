open Lexer
open Ast

(* Requires list_feature to be enabled *)

let parse_inline _ _ _ _ = None

let is_digits s =
  try
    ignore (int_of_string s);
    true
  with _ -> false

let parse_block (tokens : Lexer.token array) (pos : int)
    (_after_reference : bool) reg =
  let n = Array.length tokens in
  if pos + 1 >= n
  then None
  else
    let marker_info =
      if tokens.(pos) = Tilde && tokens.(pos + 1) = Space
      then Some (Compact, 1, 2)
      else if pos + 2 < n
      then
        match (tokens.(pos), tokens.(pos + 1), tokens.(pos + 2)) with
        | Text s, Dot, Space when is_digits s ->
            Some (OrderedCompact, int_of_string s, 3)
        | _ -> None
      else None
    in
    match marker_info with
    | None -> None
    | Some (ltype, start_num, skip) -> (
        (* collect tokens until newline *)
        let rec collect i acc =
          if i >= n || tokens.(i) = Newline
          then (List.rev acc, i)
          else collect (i + 1) (tokens.(i) :: acc)
        in
        let content_tokens, stop_pos = collect (pos + skip) [] in
        let arr = Array.of_list content_tokens in

        (* split on "  ~ " or "  [digits]. " *)
        let rec split acc current i =
          if i >= Array.length arr
          then List.rev (Array.of_list (List.rev current) :: acc)
          else
            let found_marker =
              if
                i + 3 < Array.length arr
                && arr.(i) = Space
                && arr.(i + 1) = Space
              then
                match (arr.(i + 2), arr.(i + 3)) with
                | Tilde, Space -> Some 4
                | Text s, Dot
                  when i + 4 < Array.length arr
                       && arr.(i + 4) = Space
                       && is_digits s ->
                    Some 5
                | _ -> None
              else None
            in
            match found_marker with
            | Some skip_len ->
                split (Array.of_list (List.rev current) :: acc) [] (i + skip_len)
            | None -> split acc (arr.(i) :: current) (i + 1)
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
            Some (ListNode (ltype, start_num, items), stop_pos - pos)
        | _ -> None)

let render_html _ _ _ = None
let render_tex _ _ _ = None
