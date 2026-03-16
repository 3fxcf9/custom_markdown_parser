open Ast
open Lexer

let parse_inline _ _ _ _ = None

let gather_line_no_newline (tokens : Lexer.token array) (pos : int) :
    Lexer.token array * int =
  let n = Array.length tokens in
  let rec loop i = if i >= n || tokens.(i) = Newline then i else loop (i + 1) in
  let i = loop pos in
  (Array.sub tokens pos (i - pos), i)

let split_cells line_tokens =
  let rec loop current_cell all_cells = function
    | [] -> List.rev (List.rev current_cell :: all_cells)
    | Pipe :: tl -> loop [] (List.rev current_cell :: all_cells) tl
    | t :: tl -> loop (t :: current_cell) all_cells tl
  in
  match line_tokens with
  | Pipe :: tl -> (
      let cells = loop [] [] tl in
      (* Remove last empty cell if it was terminated by Pipe *)
      match List.rev cells with
      | [] :: rest -> List.rev rest
      | _ -> cells)
  | _ -> []

let is_table_line tokens =
  let n = Array.length tokens in
  if n < 3
  then false
  else
    let starts_with_pipe_space = tokens.(0) = Pipe && tokens.(1) = Space in
    let ends_with_space_pipe =
      tokens.(n - 2) = Space && tokens.(n - 1) = Pipe
    in
    starts_with_pipe_space && ends_with_space_pipe

let extract_cells tokens =
  (* Assumes is_table_line is true *)
  let n = Array.length tokens in
  let rec loop i current_cell all_cells =
    if i >= n - 1
    then
      let cell =
        match current_cell with
        | Space :: rest -> List.rev rest
        | _ -> List.rev current_cell
      in
      List.rev (cell :: all_cells)
    else
      match (tokens.(i - 1), tokens.(i), tokens.(i + 1)) with
      | Space, Pipe, Space ->
          let cell =
            match current_cell with
            | Space :: rest -> List.rev rest
            | _ -> List.rev current_cell
          in
          loop (i + 2) [] (cell :: all_cells)
      | _ -> loop (i + 1) (tokens.(i) :: current_cell) all_cells
  in
  (* Start after the first "Pipe, Space" *)
  loop 2 [] []

let is_separator_row cells =
  let is_dash_only cell =
    match cell with
    | [] -> false
    | _ ->
        List.for_all (function Dash -> true | Space -> true | _ -> false) cell
        && List.exists (function Dash -> true | _ -> false) cell
  in
  List.for_all is_dash_only cells

let parse_block tokens pos after_reference (reg : Registry.t) =
  if pos > 0 && tokens.(pos - 1) <> Newline && not after_reference
  then None
  else
    let line_arr, next_pos = gather_line_no_newline tokens pos in
    if is_table_line line_arr
    then
      let first_row_cells = extract_cells line_arr in
      let rec collect_rows current_pos rows =
        let next_line_pos =
          if current_pos < Array.length tokens && tokens.(current_pos) = Newline
          then current_pos + 1
          else current_pos
        in
        if next_line_pos >= Array.length tokens
        then (List.rev rows, next_line_pos)
        else
          let next_line_arr, next_next_pos =
            gather_line_no_newline tokens next_line_pos
          in
          if is_table_line next_line_arr
          then collect_rows next_next_pos (extract_cells next_line_arr :: rows)
          else (List.rev rows, next_line_pos)
      in
      let other_rows, final_pos = collect_rows next_pos [] in
      (* Check if there is a separator row *)
      match other_rows with
      | sep :: body when is_separator_row sep ->
          let headers =
            List.map
              (fun c -> Parser.parse_inlines reg (Array.of_list c))
              first_row_cells
          in
          let rows =
            List.map
              (List.map (fun c -> Parser.parse_inlines reg (Array.of_list c)))
              body
          in
          Some (TableNode (headers, rows), final_pos)
      | _ ->
          let rows =
            List.map
              (List.map (fun c -> Parser.parse_inlines reg (Array.of_list c)))
              (first_row_cells :: other_rows)
          in
          Some (TableNode ([], rows), final_pos)
    else None

let render_html reg id = function
  | TableNode (headers, rows) ->
      let render_cell tag cell =
        let content =
          String.concat "" (List.map (Registry.render_html reg None) cell)
        in
        Printf.sprintf "<%s>%s</%s>" tag (String.trim content) tag
      in
      let render_row tag rows =
        let cells = List.map (render_cell tag) rows in
        Printf.sprintf "<tr>%s</tr>" (String.concat "" cells)
      in
      let header_html =
        match headers with
        | [] -> ""
        | _ -> Printf.sprintf "<thead>%s</thead>" (render_row "th" headers)
      in
      let body_html =
        let row_htmls = List.map (render_row "td") rows in
        Printf.sprintf "<tbody>%s</tbody>" (String.concat "" row_htmls)
      in
      Some (Printf.sprintf "<table%s>%s%s</table>" id header_html body_html)
  | _ -> None

let render_tex _ _ = failwith "not handled"
