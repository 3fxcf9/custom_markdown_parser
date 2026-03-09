(** Main entry point for the MDE transformation engine. *)

(** [parse_mde ?figures mde] transforms a raw MDE string into its HTML
    representation and extracts associated metadata.

    {ul
     {- [figures] : optional hash table to resolve figure references.
        - keys are filenames
        - values are the html code to replace the corresponding image with
     }
     {- [mde] : the raw input string to be parsed. }
    }

    Returns a tuple [(html, toc_html, metadata)] where [html] is the rendered
    string, [toc_html] the generated table of contents, and [metadata] is the
    YAML block found in the document ([Yaml.value option]). *)
let parse_mde ?(figures = Hashtbl.create 0) (mde : string) :
    string * string * Yaml.value option =
  let reg = Config.build_registry () in
  reg.figures <- figures;
  let tokens = Lexer.tokenize mde |> Array.of_list in
  let ast = Parser.parse reg tokens in
  let html = Registry.render_document reg ast in

  let _, _, toc_html =
    List.fold_left
      (fun (first, last_level, html) f -> f first last_level html)
      (true, 2, "<ol class=\"toc\">")
      reg.toc_setters
  in
  let toc_html = toc_html ^ "</li></ol>" in
  (html, toc_html, reg.metadata)
