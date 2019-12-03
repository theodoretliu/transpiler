type prog = stmt list

and stmt =
| Let of id * expr
| FunctionDef of id * id list * expr * bool

and expr =
| Var of id
| Constant of constant
| BinOp of expr * operator * expr
| UnaryOp of unop * expr
| LetIn of id * expr * expr
| FunctionDefIn of id * id list * expr * expr * bool
| If of expr * expr * expr
| Match of expr * (expr * expr) list
| Apply of expr * expr

and constant =
| Int of int
| Float of float
| Bool of bool
| String of string

and id = string

and operator =
| Add
| Sub
| Cons
| Append
| Concat

and unop =
| Not
| Neg
| Pos

let rec expr_to_string e indent use_newline =
  let prefix = String.make indent ' ' in
  let rest =
    match e with
    | Var i -> i
    | Constant c -> constant_to_string c
    | BinOp (e1, o, e2) ->
        (expr_to_string e1 indent false)
          ^ (operator_to_string o)
          ^ (expr_to_string e2 indent false)
    | UnaryOp (o, e) ->
        (unop_to_string o) ^ (expr_to_string e indent false)
    | LetIn (x, e1, e2) ->
        "let " ^ x ^ " =\n" ^ (expr_to_string e1 (indent + 2) true)
          ^ prefix ^ "in\n" ^ (expr_to_string e2 indent true)
    | FunctionDefIn (x, args, body, expr_in, is_rec) ->
        let args_string = String.concat " " args in
        let rec_string = if is_rec then "rec " else " " in
        let body_string = expr_to_string body (indent + 2) true in
        let in_string = expr_to_string expr_in indent true in
        "let " ^ rec_string ^ x ^ " " ^ args_string ^ "=\n" ^ body_string ^ " in\n" ^ in_string
    | If (e1, e2, e3) ->
        let e1_string = expr_to_string e1 0 false in
        let e2_string = expr_to_string e2 (indent + 2) true in
        let e3_string = expr_to_string e3 (indent + 2) true in
        "if " ^ e1_string ^ "then\n" ^ e2_string ^ prefix ^ "else\n" ^ e3_string
    | Match (match_expr, cases) ->
        let match_string = expr_to_string match_expr 0 false in
        let case_string (e1, e2) =
          let e1_s = expr_to_string e1 0 false in
          let e2_s = expr_to_string e2 (indent + 4) true in
          "| " ^ e1_s ^ " ->\n" ^ e2_s in
        let case_strings = List.map case_string cases in
        "match " ^ match_string ^ " with\n" ^ (String.concat "" case_strings)
    | Apply (e1, e2) ->
        let e1_s = expr_to_string e1 0 false in
        let e2_s = expr_to_string e2 0 false in
        e1_s ^ " " ^ e2_s in
  prefix ^ rest ^ (if use_newline then "\n" else "")

and constant_to_string =
function
| Int i -> string_of_int i
| Float f -> string_of_float f
| Bool b -> string_of_bool b
| String s -> "\"" ^ s ^ "\""

and operator_to_string =
function
| Add -> "+"
| Sub -> "-"
| Cons -> "::"
| Append -> "@"
| Concat -> "^"

and unop_to_string =
function
| Not -> "not"
| Neg -> "~-"
| Pos -> "~+"
