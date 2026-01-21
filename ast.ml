type hr_kind = Compact | Solid | Dashed | Sawteeth
type list_type = Ordered | Dash | Star | Plus | Compact

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
  | ReferenceTagNode of string
  | StrikethroughNode of node list
  | UnderlineNode of node list
