open Ast

type t = {
  mutable paragraph_stop_conditions : paragraph_stop_condition list;
  mutable block_parsers : parser_function list;
  mutable inline_parsers : parser_function list;
  mutable html_renderers : renderer_function list;
  mutable tex_renderers : renderer_function list;
}

and parser_function =
  Lexer.token array -> int -> bool -> t -> (Ast.node * int) option

and paragraph_stop_condition = Lexer.token array -> int -> bool -> t -> bool
and renderer_function = t -> string -> node -> string option
(* render reg id_string (empty or id="something") node *)

(* Renderer.  TODO: Split *)
let render_html (reg : t) (id : string option) node : string =
  let id_string =
    match id with Some s -> Printf.sprintf " id=\"%s\"" s | None -> ""
  in
  let rec try_renderer = function
    | [] -> failwith "No renderer found"
    | renderer :: tl -> (
        match renderer reg id_string node with
        | Some s -> s
        | None -> try_renderer tl)
  in
  try_renderer reg.html_renderers

let render_tex _ _ = ""

(* FIXME: Handle two consecutive referenceNode *)
let rec render_document (reg : t) (doc : node list) : string =
  match doc with
  | ReferenceTagNode next_id :: hd :: tl ->
      render_html reg (Some next_id) hd ^ render_document reg tl
  | hd :: tl -> render_html reg None hd ^ render_document reg tl
  | _ -> ""

let create_registry () : t =
  let registry : t =
    {
      paragraph_stop_conditions = [];
      block_parsers = [];
      inline_parsers = [];
      html_renderers = [];
      tex_renderers = [];
    }
  in

  (* default paragraph stop condition *)
  registry.paragraph_stop_conditions <-
    [
      (fun tokens pos _after_reference _ ->
        if pos + 1 < Array.length tokens then
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
