open Ast

type t = {
  mutable paragraph_stop_conditions : paragraph_stop_condition list;
  mutable block_parsers : parser_function list;
  mutable inline_parsers : parser_function list;
  mutable html_renderers : renderer_function list;
  mutable tex_renderers : renderer_function list;
}

and parser_function = Lexer.token array -> int -> t -> (Ast.node * int) option
and paragraph_stop_condition = Lexer.token array -> int -> bool
and renderer_function = t -> node -> string option

(* Renderer.  TODO: Split *)
let render_html (reg : t) node : string =
  let rec try_renderer = function
    | [] -> failwith "No renderer found"
    | renderer :: tl -> (
        match renderer reg node with Some s -> s | None -> try_renderer tl)
  in
  try_renderer reg.html_renderers

let render_tex _ _ = ""

let render_document (reg : t) (doc : node list) : string =
  doc |> List.map (render_html reg) |> String.concat ""

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
      (fun tokens pos ->
        if pos + 1 < Array.length tokens then
          match (tokens.(pos), tokens.(pos + 1)) with
          | Newline, Newline -> true
          | _ -> false
        else false);
    ];

  (* default HTML renderers *)
  registry.html_renderers <-
    [
      (fun reg node ->
        match node with
        | ParagraphNode children ->
            let results = List.map (render_html reg) children in
            Some ("<p>" ^ String.concat "" results ^ "</p>")
        | _ -> None);
    ];

  registry.html_renderers <-
    (fun _reg node -> match node with TextNode s -> Some s | _ -> None)
    :: registry.html_renderers;

  registry
