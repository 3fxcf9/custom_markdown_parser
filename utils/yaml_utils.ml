let rec merge_yaml (v1 : Yaml.value) (v2 : Yaml.value) : Yaml.value =
  match (v1, v2) with
  | `O dict1, `O dict2 ->
      let res =
        List.fold_left
          (fun acc (k, v) ->
            if List.mem_assoc k acc
            then
              let old_v = List.assoc k acc in
              let new_acc = List.remove_assoc k acc in
              (k, merge_yaml old_v v) :: new_acc
            else (k, v) :: acc)
          dict1 dict2
      in
      `O res
  | `A l1, `A l2 -> `A (l1 @ l2)
  | _ -> v2

let merge_yaml_options opt1 opt2 =
  match (opt1, opt2) with
  | Some v1, Some v2 -> Some (merge_yaml v1 v2)
  | Some v1, None -> Some v1
  | None, Some v2 -> Some v2
  | None, None -> None
