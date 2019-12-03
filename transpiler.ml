let rec repeat n elt =
  if n = 0 then []
  else elt :: repeat (n - 1) elt ;;

let depth = ref 0 ;;

let f =
  fun lexbuf ->
    let rec f' lexbuf =
      match PythonLexer.token lexbuf with
      | SPACES i ->
          let new_depth = i / 4 in
          let depth_delta = new_depth - !depth in
          depth := new_depth;
          if depth_delta < 0 then repeat ~-depth_delta PythonParser.DEDENT
          else if depth_delta > 0 then repeat depth_delta PythonParser.INDENT
          else f' lexbuf
      | ENDMARKER ->
          let prev_depth = !depth in
          depth := 0;
          repeat prev_depth PythonParser.DEDENT @ [NEWLINE; ENDMARKER]
      | e -> [e] in
    f' lexbuf


let xs = ref [] ;;

let final =
  fun lexbuf ->
    match !xs with
    | h :: t -> xs := t; h
    | [] ->
        match f lexbuf with
        | [] -> failwith "somehow didn't get nothing!"
        | h :: t -> xs := t; h ;;

let reset () =
  depth := 0;
  xs := [] ;;

let python_ast_from_file file_name =
  reset () ;
  let channel = open_in file_name in
  let lexbuf = Lexing.from_channel channel in
  PythonParser.file_input final lexbuf ;;

let python_ast_from_string s =
  reset () ;
  let lexbuf = Lexing.from_string (s ^ "\n") in
  PythonParser.file_input final lexbuf ;;

let rec all_tokens_helper lexbuf =
  let output = final lexbuf in
  if output = PythonParser.ENDMARKER then [output]
  else output :: all_tokens_helper lexbuf ;;

let all_tokens_of_string s =
  reset () ;
  all_tokens_helper (Lexing.from_string s) ;;

let all_tokens_of_file filename =
  reset () ;
  let channel = open_in filename in
  Lexing.from_channel channel |> all_tokens_helper ;;
