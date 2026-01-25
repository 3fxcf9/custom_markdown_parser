type hr_kind = Compact | Solid | Dashed | Sawteeth
type list_type = Ordered | Dash | Star | Plus | Compact

let translate_shortcut_fr =
  [
    ("thm", "théorème");
    ("prop", "proposition");
    ("cor", "corollaire");
    ("lemma", "lemme");
    ("property", "propriété");
    ("notation", "notation");
    ("proof", "preuve");
    ("rem", "remarque");
    ("def", "définition");
    ("defprop", "définition-proposition");
    ("eg", "exemple");
    ("method", "méthode");
    ("exercise", "exercice");
    ("quote", "citation");
    ("fig", "figure");
    ("lfig", "figure");
    ("rfig", "figure");
    ("callout", "encadré");
  ]

let environment_display_name environment_name =
  try List.assoc environment_name translate_shortcut_fr
  with _ -> environment_name

(* type node = .. *)
(* type node += TextNode of string | ParagraphNode of node list *)
type node =
  | TextNode of string
  | ParagraphNode of node list
  | BoldNode of node list
  | CodeBlockNode of string option * string
  | CodeInlineNode of string
  | EnvironmentNode of string * string option * node list
  | FootnoteNode of node list
  | HeadingNode of int * node list
  | HighlightNode of node list
  | HRuleNode of hr_kind
  | ItalicNode of node list
  | LinkNode of (node list * string)
  | ListNode of list_type * int * node list list
  | MathDisplayNode of string
  | MathInlineNode of string
  | NbspNode of bool
  | StrikethroughNode of node list
  | UnderlineNode of node list
  | ImageNode of string
  | ReferenceNode of string
  | ReferenceTagNode of string

let rec string_of_node node =
  match node with
  | TextNode s -> Printf.sprintf "[TEXT:%s]" s
  | ParagraphNode lst -> Printf.sprintf "[PARA:%s]" (string_of_nodes lst)
  | BoldNode lst -> Printf.sprintf "[BOLD:%s]" (string_of_nodes lst)
  | CodeBlockNode (lang_opt, code) -> (
      match lang_opt with
      | Some lang -> Printf.sprintf "[CODEBLOCK:%s:%s]" lang code
      | None -> Printf.sprintf "[CODEBLOCK:%s]" code)
  | CodeInlineNode code -> Printf.sprintf "[CODE:%s]" code
  | EnvironmentNode (name, opt_arg, content) ->
      let opt_str = match opt_arg with Some s -> s | None -> "_" in
      Printf.sprintf "[ENV:%s(%s):%s]" name opt_str (string_of_nodes content)
  | FootnoteNode lst -> Printf.sprintf "[FOOT:%s]" (string_of_nodes lst)
  | HeadingNode (level, lst) ->
      Printf.sprintf "[HEAD%d:%s]" level (string_of_nodes lst)
  | HighlightNode lst -> Printf.sprintf "[HILITE:%s]" (string_of_nodes lst)
  | HRuleNode _ -> "[HR]"
  | ItalicNode lst -> Printf.sprintf "[ITALIC:%s]" (string_of_nodes lst)
  | LinkNode (lst, url) ->
      Printf.sprintf "[LINK:%s→%s]" (string_of_nodes lst) url
  | ListNode (_, _, items) ->
      let item_strs = List.map string_of_nodes items in
      Printf.sprintf "[LIST:%s]" (String.concat "; " item_strs)
  | MathDisplayNode s -> Printf.sprintf "[MATH-DISPLAY:%s]" s
  | MathInlineNode s -> Printf.sprintf "[MATH-INLINE:%s]" s
  | NbspNode _ -> "[NBSP]"
  | StrikethroughNode lst -> Printf.sprintf "[STRIKE:%s]" (string_of_nodes lst)
  | UnderlineNode lst -> Printf.sprintf "[UNDER:%s]" (string_of_nodes lst)
  | ImageNode s -> Printf.sprintf "[IMAGE:%s]" s
  | ReferenceNode s -> Printf.sprintf "[REF:%s]" s
  | ReferenceTagNode s -> Printf.sprintf "[REF-TAG:%s]" s

and string_of_nodes lst = String.concat " " (List.map string_of_node lst)

let debug_nodes nodes =
  nodes |> List.map string_of_node |> String.concat "\n" |> print_endline
