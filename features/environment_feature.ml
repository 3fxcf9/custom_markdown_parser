open Ast
open Lexer

type node += EnvironmentNode of string * string option * node list

let allowed_envs =
  [
    "thm";
    "prop";
    "cor";
    "lemma";
    "property";
    "notation";
    "proof";
    "def";
    "method";
    "rem";
    "eg";
    "exercise";
    "fold";
    "quote";
    "fig";
    "lfig";
    "rfig";
    "offprog";
    "callout";
  ]

(* gather tokens for a single line (including trailing Newline if present) *)
let gather_line (tokens : Lexer.token array) (pos : int) :
    Lexer.token array * int =
  let n = Array.length tokens in
  let rec loop i = if i >= n || tokens.(i) = Newline then i else loop (i + 1) in
  let i = loop pos in
  if i >= n then (Array.sub tokens pos (i - pos), i - pos)
  else (Array.sub tokens pos (i - pos + 1), i - pos + 1)

(* Append an array slice [start..start+len-1] to a rev-list of tokens *)
let append_array_slice_to_rev_list arr start len acc_rev =
  let rec loop j acc =
    if j >= start + len then acc else loop (j + 1) (arr.(j) :: acc)
  in
  loop start acc_rev

(* Detect an env-open line starting at pos.
   Syntax: %Text(type) [rest-of-line-is-title] NEWLINE
   Returns Some (env_short, title_opt, pos_after_open_line) or None.
*)
let parse_env_open_line tokens pos =
  let n = Array.length tokens in
  if pos >= n then None
  else
    match tokens.(pos) with
    | Percent -> (
        if pos + 1 >= n then None
        else
          match tokens.(pos + 1) with
          | Text name when List.mem name allowed_envs ->
              (* collect until newline *)
              let rec loop i =
                if i >= n then (i, [||])
                else
                  match tokens.(i) with
                  | Newline ->
                      (i + 1, Array.sub tokens (pos + 2) (i - (pos + 2)))
                  | _ -> loop (i + 1)
              in
              let next_pos, title_tokens = loop (pos + 2) in
              let title =
                let s =
                  Array.fold_left
                    (fun acc t -> acc ^ Lexer.token_to_literal t)
                    "" title_tokens
                  |> String.trim
                in
                if s = "" then None else Some s
              in
              Some (name, title, next_pos)
          | _ -> None)
    | _ -> None

let rec collect_indented_lines tokens pos indent_min acc_rev =
  let n = Array.length tokens in
  if pos >= n then (acc_rev, pos)
  else
    match tokens.(pos) with
    | Indent lvl when lvl >= indent_min ->
        let line_arr, consumed = gather_line tokens pos in
        if Array.length line_arr = 0 then
          collect_indented_lines tokens (pos + consumed) indent_min acc_rev
        else
          (* adjust leading indent token if present *)
          let adjusted_list =
            match line_arr.(0) with
            | Indent lvl when lvl >= indent_min ->
                let new_lvl = lvl - indent_min in
                if new_lvl > 0 then (
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

let parse_inline _ _ _ = None

let parse_block tokens pos reg =
  match parse_env_open_line tokens pos with
  | None -> None
  | Some (env_name, title_opt, after_open_pos) -> (
      (* collect content lines (allow blank lines) *)
      let acc_rev = [] in
      let acc_rev', pos_after =
        collect_indented_lines tokens after_open_pos 2 acc_rev
      in
      match tokens.(pos_after) with
      | Percent ->
          let tokens = List.rev acc_rev' |> Array.of_list in
          let parsed =
            if Array.length tokens = 0 then [] else Parser.parse reg tokens
          in
          Some (EnvironmentNode (env_name, title_opt, parsed), pos_after + 2)
      | _ -> None)

let render_tex _ _ = None

let render_html reg = function
  | EnvironmentNode (env_short, title_opt, content_nodes) ->
      let title_html =
        match title_opt with
        | None -> ""
        | Some t -> Printf.sprintf "<div class=\"environment-title\">%s</div>" t
      in
      let content_html =
        String.concat "" (List.map (Registry.render_html reg) content_nodes)
      in
      let render_figure class_name =
        if title_html <> "" then
          Printf.sprintf
            "<figure class=\"%s\">%s<figcaption>%s</figcaption></figure>"
            class_name content_html
            (match title_opt with Some s -> s | None -> "")
        else
          Printf.sprintf "<figure class=\"%s\">%s</figure>" class_name
            content_html
      in
      let render_normal env =
        let _translate_shortcut =
          [
            ("thm", "theorem");
            ("prop", "proposition");
            ("cor", "corollary");
            ("rem", "remark");
            ("def", "definition");
            ("eg", "example");
          ]
        in
        let translate_shortcut_fr =
          [
            ("thm", "théorème");
            ("prop", "proposition");
            ("cor", "corollaire");
            ("lemma", "lemme");
            ("rem", "remarque");
            ("def", "définition");
            ("eg", "exemple");
            ("method", "méthode");
            ("property", "propriété");
          ]
        in
        let lookup map key = try List.assoc key map with _ -> key in
        (* NOTE: Change `translate_shortcut_fr` to `translate_shortcut` below to use english name *)
        let env_name = lookup translate_shortcut_fr env in
        let title_section =
          match env with
          | "callout" ->
              Printf.sprintf
                "<div class=\"environment-title\">Callout — %s</div>"
                (String.capitalize_ascii env_name)
          | e
            when List.mem e
                   [
                     "thm";
                     "prop";
                     "cor";
                     "lemma";
                     "property";
                     "def";
                     "method";
                     "notation";
                   ] -> (
              match title_opt with
              | Some t ->
                  Printf.sprintf
                    "<div class=\"environment-title\">%s — %s</div>"
                    (String.capitalize_ascii env_name)
                    t
              | None ->
                  Printf.sprintf "<div class=\"environment-title\">%s</div>"
                    (String.capitalize_ascii env_name))
          | _ -> ""
        in
        Printf.sprintf "<div class=\"environment environment-%s\">%s%s</div>"
          env_name title_section content_html
      in
      let out =
        match env_short with
        | "offprog" ->
            Printf.sprintf "<div class=\"off-program\">%s</div>" content_html
        | "fig" -> render_figure ""
        | "lfig" -> render_figure "float-left"
        | "rfig" -> render_figure "float-right"
        | "quote" ->
            if title_html <> "" then
              Printf.sprintf
                "<div \
                 class=\"blockquote\"><blockquote>%s</blockquote><cite>%s</cite></div>"
                content_html
                (match title_opt with Some s -> s | None -> "")
            else
              Printf.sprintf
                "<div class=\"blockquote\"><blockquote>%s</blockquote></div>"
                content_html
        | "fold" ->
            let summary =
              match title_opt with Some s -> s | None -> "View more"
            in
            Printf.sprintf "<details>%s<summary>%s</summary></details>"
              content_html summary
        | other -> render_normal other
      in
      Some out
  | _ -> None
