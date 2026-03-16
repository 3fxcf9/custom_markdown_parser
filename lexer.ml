(** Lexer for MDE syntax. *)

type token =
  | Newline
  | Space
  | Dot
  | Star
  | Plus
  | Equal
  | Dash
  | Slash
  | Underscore
  | Dollar
  | Hash
  | At
  | Percent
  | Tilde
  | Caret
  | Colon
  | Backtick
  | Langle
  | Rangle
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lcurly
  | Rcurly
  | Pipe
  | Indent of int
  | Text of string

(** [debug_token token] Prints a token in a debug representation. *)
let debug_token token =
  print_string
  @@
  match token with
  | Newline -> "[NEWLINE]"
  | Space -> "[SPACE]"
  | Dot -> "[.]"
  | Star -> "[*]"
  | Plus -> "[+]"
  | Equal -> "[=]"
  | Dash -> "[-]"
  | Slash -> "[/]"
  | Underscore -> "[_]"
  | Dollar -> "[$]"
  | Hash -> "[#]"
  | At -> "[@]"
  | Percent -> "[%]"
  | Tilde -> "[~]"
  | Caret -> "[^]"
  | Colon -> "[:]"
  | Backtick -> "[`]"
  | Langle -> "[<]"
  | Rangle -> "[>]"
  | Lparen -> "[(]"
  | Rparen -> "[)]"
  | Lbracket -> "[DELIM:[]"
  | Rbracket -> "[DELIM:]]"
  | Lcurly -> "[{]"
  | Rcurly -> "[}]"
  | Pipe -> "[|]"
  | Indent level -> Printf.sprintf "[INDENT:%d]" level
  | Text s -> Printf.sprintf "[TEXT:%s]" s

(** [token_to_literal token] Returns the original string representation of a
    token. *)
let token_to_literal token =
  match token with
  | Newline -> "\n"
  | Space -> " "
  | Dot -> "."
  | Star -> "*"
  | Plus -> "+"
  | Equal -> "="
  | Dash -> "-"
  | Slash -> "/"
  | Underscore -> "_"
  | Dollar -> "$"
  | Hash -> "#"
  | At -> "@"
  | Percent -> "%"
  | Tilde -> "~"
  | Caret -> "^"
  | Colon -> ":"
  | Backtick -> "`"
  | Langle -> "<"
  | Rangle -> ">"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Lcurly -> "{"
  | Rcurly -> "}"
  | Pipe -> "|"
  | Indent level -> Bytes.make level ' ' |> Bytes.to_string
  | Text s -> s

(** [tokenize input] Converts a string into a list of tokens, handling
    indentation and comments. *)
let tokenize input =
  let tokens = ref [] in
  let current_text = ref "" in
  let indent_level = ref 0 in

  (* Mapping character to token kind *)
  let rune_to_token ch =
    match ch with
    | '\n' -> Some Newline
    | ' ' -> Some Space
    | '.' -> Some Dot
    | '*' -> Some Star
    | '+' -> Some Plus
    | '=' -> Some Equal
    | '-' -> Some Dash
    | '/' -> Some Slash
    | '_' -> Some Underscore
    | '$' -> Some Dollar
    | '#' -> Some Hash
    | '@' -> Some At
    | '%' -> Some Percent
    | '~' -> Some Tilde
    | '^' -> Some Caret
    | ':' -> Some Colon
    | '`' -> Some Backtick
    | '<' -> Some Langle
    | '>' -> Some Rangle
    | '(' -> Some Lparen
    | ')' -> Some Rparen
    | '[' -> Some Lbracket
    | ']' -> Some Rbracket
    | '{' -> Some Lcurly
    | '}' -> Some Rcurly
    | '|' -> Some Pipe
    | _ -> None
  in

  let input_len = String.length input in

  let add_token kind = tokens := kind :: !tokens in

  let add_text_token () =
    if String.length !current_text > 0
    then (
      add_token (Text !current_text);
      current_text := "")
  in

  let rec skip_comment j =
    if j >= input_len
    then j
    else if input.[j] = '\n'
    then j + 1
    else skip_comment (j + 1)
  in

  let rec process_char i =
    if i >= input_len
    then ()
    else
      let ch = input.[i] in
      if !indent_level >= 0 && ch = ' '
      then (
        incr indent_level;
        process_char (i + 1))
      else if
        i + 1 < input_len
        && ch = '/'
        && input.[i + 1] = '/'
        && (i = 0 || input.[i - 1] = ' ' || input.[i - 1] = '\n')
      then (
        indent_level := 0;
        skip_comment (i + 2) |> process_char)
      else if !indent_level > 0
      then (
        add_token (Indent !indent_level);
        indent_level := -1;
        process_char i)
      else (
        indent_level := -1;
        match rune_to_token ch with
        | Some t ->
            add_text_token ();
            add_token t;
            if t = Newline then indent_level := 0;
            process_char (i + 1)
        | None ->
            current_text := !current_text ^ String.make 1 ch;
            process_char (i + 1))
  in
  process_char 0;
  add_text_token ();
  List.rev !tokens
