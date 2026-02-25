(** Central configuration for the parser and renderer. *)

open Ast

type t = {
  mutable paragraph_stop_conditions : paragraph_stop_condition list;
  mutable block_parsers : parser_function list;
  mutable inline_parsers : parser_function list;
  mutable html_renderers : renderer_function list;
  mutable tex_renderers : renderer_function list;
  mutable heading_position : int array;
  mutable environment_count : (string, int) Hashtbl.t;
  mutable references : (string, string) Hashtbl.t;
  mutable figures : (string, string) Hashtbl.t;
  mutable metadata : Yaml.value option;
  mutable toc_setters : (bool -> int -> string -> bool * int * string) list;
  mutable update_toc : string -> unit;
}

and parser_function =
  Lexer.token array -> int -> bool -> t -> (Ast.node * int) option

and paragraph_stop_condition = Lexer.token array -> int -> bool -> t -> bool
and renderer_function = t -> string -> node -> string option
(* render reg id_string (empty or id="something") node *)

(* TODO: Sketchy solution *)
let rec repeat s = function 1 -> s | n -> s ^ repeat s (n - 1)

let add_to_toc reg level id heading_html =
  let f (first : bool) (last : int) (html : string) =
    let link_html = Printf.sprintf "<a href=\"#%s\">%s</a>" id heading_html in
    if level < 2
    then (true, last, html)
    else if level > last
    then
      let html = if first then html ^ "<li>" else html in
      ( false,
        level,
        html
        ^ Printf.sprintf "%s%s" (repeat "<ol>\n<li>" (level - last)) link_html
      )
    else if level < last
    then
      let close =
        if last - level > 1 then repeat "</li></ol>" (last - level - 1) else ""
      in
      ( false,
        level,
        html ^ Printf.sprintf "%s</li></ol></li>\n<li>%s" close link_html )
    else if first
    then (false, level, html ^ Printf.sprintf "\n<li>%s" link_html)
    else (false, level, html ^ Printf.sprintf "</li>\n<li>%s" link_html)
  in
  reg.toc_setters <- f :: reg.toc_setters

(** [render_html reg id node] Renders a node with the first compatible
    registered HTML renderer. *)
let render_html (reg : t) (id : string option) node : string =
  let id_string =
    match (id, node) with
    | Some s, HeadingNode (level, _, _) ->
        reg.update_toc <- add_to_toc reg level s;
        Printf.sprintf " id=\"%s\"" s
    | Some s, _ -> Printf.sprintf " id=\"%s\"" s
    | None, HeadingNode (level, _, pos) ->
        let id = Digest.string pos |> Digest.to_hex in
        reg.update_toc <- add_to_toc reg level id;
        Printf.sprintf " id=\"%s\"" id
    | _ -> ""
  in
  let rec try_renderer = function
    | [] -> failwith "No renderer found"
    | renderer :: tl -> (
        match renderer reg id_string node with
        | Some s -> s
        | None -> try_renderer tl)
  in
  try_renderer reg.html_renderers

(** TODO: [render_tex reg id node] Renders a node with the first compatible
    registered Tex renderer. *)
let render_tex _ _ = ""

(** [render_document reg doc] Converts a list of AST nodes into a full HTML
    string. *)
let rec render_document (reg : t) (doc : node list) : string =
  (* FIXME: Handle two consecutive referenceNode *)
  match doc with
  | ReferenceTagNode next_id :: hd :: tl ->
      render_html reg (Some next_id) hd ^ render_document reg tl
  | EmptyNode :: tl -> render_document reg tl
  | hd :: tl -> render_html reg None hd ^ render_document reg tl
  | _ -> ""

(** [create_registry ()] Initializes a registry with default paragraph and text
    renderers. *)
let create_registry () : t =
  let registry : t =
    {
      paragraph_stop_conditions = [];
      block_parsers = [];
      inline_parsers = [];
      html_renderers = [];
      tex_renderers = [];
      heading_position = Array.make 7 0;
      environment_count = Hashtbl.create 16;
      references = Hashtbl.create 16;
      figures = Hashtbl.create 16;
      metadata = None;
      toc_setters = [];
      update_toc = (fun _ -> ());
    }
  in

  (* default paragraph stop condition *)
  registry.paragraph_stop_conditions <-
    [
      (fun tokens pos _after_reference _ ->
        if pos + 1 < Array.length tokens
        then
          match (tokens.(pos), tokens.(pos + 1)) with
          | Newline, Newline -> true
          | _ -> false
        else false);
    ];

  (* default HTML renderers *)
  registry.html_renderers <-
    [
      (fun reg id node ->
        match node with
        | ParagraphNode children ->
            let results = List.map (render_html reg None) children in
            Some (Printf.sprintf "<p%s>%s</p>" id (String.concat "" results))
        | _ -> None);
    ];

  registry.html_renderers <-
    (fun _reg _id node -> match node with TextNode s -> Some s | _ -> None)
    :: registry.html_renderers;

  registry
