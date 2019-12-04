let rec repeat n elt =
  if n = 0 then []
  else elt :: repeat (n - 1) elt ;;

let depth = ref 0 ;;

let f =
  fun lexbuf ->
    let rec f' lexbuf =
      match PythonLexer.token lexbuf with
      | SPACES i ->
          let new_depth = (i - 1) / 4 in
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

let genvar =
  let counter = ref 0 in
  fun prefix ->
    let prev = !counter in
    counter := prev + 1;
    prefix ^ (string_of_int prev)

let rec compile_prog (p : PythonAst.prog) : OcamlAst.prog =
  List.map compile_stmt p

and fold_stmts stmts =
  List.fold_right (fun stmt acc ->
  match stmt with
  | OcamlAst.Let (i, e) -> OcamlAst.LetIn (i, e, acc)
  | FunctionDef (f, args, body, is_rec) ->
      FunctionDefIn (f, args, body, acc, is_rec)
  | Expr e -> e) stmts (Constant Unit)

and compile_stmt (s : PythonAst.stmt) : OcamlAst.stmt =
  match s with
  | Assign ([Name x], e) -> Let (x, compile_expr e)
  | Return (Some e) -> Expr (compile_expr e)
  | Return (None) -> Expr (Constant Unit)
  | FunctionDef (f, args, body, [], None, None) ->
      let stmt_list = compile_prog body in
      let actual_body =
        List.fold_right (fun stmt acc ->
          match stmt with
          | OcamlAst.Let (i, e) -> OcamlAst.LetIn (i, e, acc)
          | FunctionDef (f, args, body, is_rec) ->
              FunctionDefIn (f, args, body, acc, is_rec)
          | Expr e -> e) stmt_list (Constant Unit) in
      FunctionDef (f, args, actual_body, true)
  | If (t,
        [Assign ([Name x], ife)],
        [Assign ([Name y], elsee)]) ->
      if x <> y then failwith "not supported" else
      let ifbranch = compile_expr ife in
      let elsebranch = compile_expr elsee in
      let condition = compile_expr t in
      Let (x, If (condition, ifbranch, elsebranch))

  | For (Name x, Name l, [Assign([Name s], e)], [], None) ->
      let compiled_update = compile_expr e in
      Let (s,
        FunctionDefIn (
          "helper",
          [l; s],
          Match (
            Var l, [
              List [], Var s ;
              BinOp (Var x, Cons, Var "tl"), Apply (Apply (Var "helper", Var "tl"), compiled_update)
            ]
          ),
          Apply (Apply (Var "helper", Var l), Var s),
          true
        )
      )
  | Expr e -> Expr (compile_expr e)
  | _ -> failwith "unsupported"

and compile_expr (e : PythonAst.expr) : OcamlAst.expr =
  match e with
  | Name x -> Var x
  | List es -> List (List.map compile_expr es)
  | Tuple es -> List (List.map compile_expr es)
  | BinOp (e1, o, e2) ->
      BinOp (compile_expr e1, compile_operator o, compile_expr e2)
  | UnaryOp (o, e) ->
      UnaryOp (compile_unop o, compile_expr e)
  | Call (Name f, args, _) ->
      List.fold_left (fun acc arg ->
        OcamlAst.Apply (acc, compile_expr arg)) (Var f) args
  | Constant c ->
      let cc = match c with
      | String s -> OcamlAst.String s
      | Int i -> Int i
      | Bool b -> Bool b
      | NoneType | Ellipsis -> failwith "not supported" in
      Constant cc
  | BoolOp (op, [e1; e2]) ->
      BinOp (compile_expr e1, compile_boolop op, compile_expr e2)
  | _ -> failwith "not supported"

and compile_boolop o =
  match o with
  | And -> And
  | Or -> Or

and compile_operator o =
  match o with
  | Add -> Add
  | Sub -> Sub
  | Mult -> Mult
  | Div -> Div
  | _ -> failwith "not supported"

and compile_unop =
  function
  | Not -> Not
  | USub -> Neg
  | UAdd -> Pos
  | _ -> failwith "not supported"
