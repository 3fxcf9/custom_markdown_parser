module type FEATURE = sig
  val paragraph_stop_condition : Registry.paragraph_stop_condition
  val parse_block : Registry.parser_function
  val parse_inline : Registry.parser_function
  val render_html : Registry.renderer_function
  val render_tex : Registry.renderer_function
end
