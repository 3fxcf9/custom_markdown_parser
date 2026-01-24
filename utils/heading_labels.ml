let upper_letter_of_int n =
  if n < 1 || n > 26 then invalid_arg "upper_letter_of_int";
  String.make 1 (Char.chr (Char.code 'A' + n - 1))

let lower_letter_of_int n =
  if n < 1 || n > 26 then invalid_arg "lower_letter_of_int";
  String.make 1 (Char.chr (Char.code 'a' + n - 1))

let heading_of_array (a : int array) : string =
  if Array.length a <> 7 then invalid_arg "heading_of_array: expected length 7";
  let parts = ref [] in

  (* index 4–6: decimal numbers *)
  let add_dec idx =
    let v = a.(idx) in
    if v <> 0 then parts := string_of_int v :: !parts
  in
  add_dec 6;
  add_dec 5;
  add_dec 4;

  (* index 3: lowercase letter (a, b, ...) *)
  let v3 = a.(3) in
  if v3 <> 0 then parts := lower_letter_of_int v3 :: !parts;

  (* index 2: uppercase letter (A, B, ...) *)
  let v2 = a.(2) in
  if v2 <> 0 then parts := upper_letter_of_int v2 :: !parts;

  (* index 1: roman (I, II, ...) *)
  let v1 = a.(1) in
  if v1 <> 0 then parts := Roman.to_roman v1 :: !parts;

  String.concat "." !parts
