let prog =
  let in_filename = Sys.argv.(1) in
  let python_ast = Transpiler.python_ast_from_file in_filename in
  let ocaml_ast = Transpiler.compile_prog python_ast in
  let ocaml_str = OcamlAst.prog_to_string ocaml_ast in
  Printf.printf "%s" ocaml_str
