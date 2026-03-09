let pprint_yaml_opt (ov : Yaml.value option) =
  match ov with
  | None -> print_endline "None"
  | Some v -> (
      match Yaml.to_string v with
      | Ok s -> print_string s
      | Error (`Msg m) -> Printf.eprintf "Error encoding YAML: %s\n" m)

let () =
  let content = In_channel.with_open_bin "test.mde" In_channel.input_all in
  let html, toc, meta = Mde_parser.parse_mde content in
  pprint_yaml_opt meta;
  print_newline ();
  print_endline (toc ^ html)
