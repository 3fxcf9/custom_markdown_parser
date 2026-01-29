(** Abstract Syntax Tree nodes and utilities. *)

type hr_kind = Compact | Solid | Dashed | Sawteeth
type list_type = Ordered | Dash | Star | Plus | Compact

let translate_environment_names =
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

(** [environment_display_name name] Translates internal environment tags to
    French labels (e.g., "thm" to "théorème"). *)
let environment_display_name environment_name =
  try List.assoc environment_name translate_environment_names
  with _ -> environment_name

type node =
  | EmptyNode
  | TextNode of string
  | ParagraphNode of node list
  | BoldNode of node list
  | CodeBlockNode of string option * string
  | CodeInlineNode of string
  | EnvironmentNode of string * node list option * node list
  | SidenoteNode of node list
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

(** {1:internal Internal Utilities}
    The following functions are used for debugging and are not required for
    general use. *)

let rec string_of_node node =
  match node with
  | EmptyNode -> Printf.sprintf "[NONE]"
  | TextNode s -> Printf.sprintf "[TEXT:%s]" s
  | ParagraphNode lst -> Printf.sprintf "[PARA:%s]" (string_of_nodes lst)
  | BoldNode lst -> Printf.sprintf "[BOLD:%s]" (string_of_nodes lst)
  | CodeBlockNode (lang_opt, code) -> (
      match lang_opt with
      | Some lang -> Printf.sprintf "[CODEBLOCK:%s:%s]" lang code
      | None -> Printf.sprintf "[CODEBLOCK:%s]" code)
  | CodeInlineNode code -> Printf.sprintf "[CODE:%s]" code
  | EnvironmentNode (name, opt_arg, content) ->
      let opt_nodes =
        match opt_arg with Some s -> s | None -> [ EmptyNode ]
      in
      Printf.sprintf "[ENV:%s(%s):%s]" name
        (string_of_nodes opt_nodes)
        (string_of_nodes content)
  | SidenoteNode lst -> Printf.sprintf "[SIDE:%s]" (string_of_nodes lst)
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

(** [debug_nodes nodes] Prints a string representation of the AST to stdout. *)
let debug_nodes nodes =
  nodes |> List.map string_of_node |> String.concat "\n" |> print_endline
