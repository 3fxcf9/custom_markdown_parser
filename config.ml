open Registry

let init_feature (registry : Registry.t) (feature : (module Feature.FEATURE)) =
  let module F = (val feature : Feature.FEATURE) in
  registry.paragraph_stop_conditions <-
    (fun tokens pos reg -> F.parse_block tokens pos reg |> Option.is_some)
    :: registry.paragraph_stop_conditions;
  registry.block_parsers <- F.parse_block :: registry.block_parsers;
  registry.inline_parsers <- F.parse_inline :: registry.inline_parsers;
  registry.html_renderers <- F.render_html :: registry.html_renderers;
  registry.tex_renderers <- F.render_tex :: registry.tex_renderers

let build_registry () =
  let reg = Registry.create_registry () in
  (* Parsing order: last to first *)
  let features : (module Feature.FEATURE) list =
    [
      (module Nbsp_feature);
      (module Italic_feature);
      (module Bold_feature);
      (module Underline_feature);
      (module Highlight_feature);
      (module Strikethrough_feature);
      (module Hrule_feature);
      (module Code_inline);
      (module Math_inline);
      (module Math_display);
      (module List_feature);
      (* IMPORTANT: Compact list must be parsed before List_feature *)
      (module Compact_list_feature);
      (module Environment_feature);
      (module Heading_feature);
    ]
  in
  List.iter (init_feature reg) features;
  reg
