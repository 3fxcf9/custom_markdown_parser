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

    Returns a tuple [(html, metadata)] where [html] is the rendered string and
    [metadata] is the YAML block found in the document. *)
let parse_mde ?(figures = Hashtbl.create 0) (mde : string) =
  let reg = Config.build_registry () in
  reg.figures <- figures;
  let tokens = Lexer.tokenize mde |> Array.of_list in
  let ast = Parser.parse reg tokens in
  (Registry.render_document reg ast, reg.metadata)
