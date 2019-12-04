%{
open PythonAst
%}

/* Declare your tokens here. */
%token ENDMARKER
%token NEWLINE
%token AT "@"
%token ASYNC
%token <string> NAME
%token LPAREN "("
%token RPAREN ")"
%token DEF "def"
%token COLON ":"
%token RARROW "->"
%token DOT "."
%token WHILE "while"
%token INDENT
%token DEDENT
%token COMMA ","
%token COLONEQUAL ":="
%token EQUAL "="
%token DOUBLESTAR "**"
%token STAR "*"
%token OR "or"
%token AND "and"
%token NOT "not"
%token LESS "<"
%token GREATER ">"
%token EQEQUAL "=="
%token LESSEQUAL "<="
%token GREATEREQUAL ">="
%token LESSGREATER "<>"
%token NOTEQUAL "!="
%token IN "in"
%token IS "is"
%token SEMICOLON ";"
%token PLUSEQUAL "+="
%token MINUSEQUAL "-="
%token STAREQUAL "*="
%token ATEQUAL "@="
%token SLASHEQUAL "/="
%token PERCENTEQUAL "%="
%token ANDEQUAL "&="
%token VBAREQUAL "|="
%token CIRCUMFLEXEQUAL "^="
%token LEFTSHIFTEQUAL "<<="
%token RIGHTSHIFTEQUAL ">>="
%token DOUBLESTAREQUAL "**="
%token DOUBLESLASH "//"
%token DOUBLESLASHEQUAL "//="
%token YIELD "yield"
%token FROM "from"
%token VBAR "|"
%token CIRCUMFLEX "^"
%token AMPER "&"
%token LEFTSHIFT "<<"
%token RIGHTSHIFT ">>"
%token PLUS "+"
%token MINUS "-"
%token SLASH "/"
%token PERCENT "%"
%token TILDE "~"
%token LSQB "["
%token RSQB "]"
%token LBRACE "{"
%token RBRACE "}"
%token ELLIPSIS "..."
%token NONE "None"
%token TRUE "True"
%token FALSE "False"
%token <int> NUMBER
%token <string> STRING
%token FOR "for"
%token ELSE "else"
%token ELIF "elif"
%token IF "if"
%token <int> SPACES

%token PASS "pass"
%token BREAK "break"
%token CONTINUE "continue"
%token RETURN "return"

%token UNARY_DUMMY

/* ---------------------------------------------------------------------- */
/* %start single_input */
%start file_input
/* %start eval_input */

/* %type <md> single_input */
%type <stmt list> stmt_newline
%type <PythonAst.prog> file_input
/* %type <md> eval_input */

%type <stmt> async_funcdef
%type <stmt> funcdef

%type <arguments> parameters
%type <arguments> typedargslist

%type <stmt list> func_body_suite
%type <stmt> if_stmt
%type <stmt> while_stmt
%type <stmt list> simple_stmt
%type <stmt> small_stmt
%type <stmt list> stmt

%type <expr> yield_expr

%type <expr> expr
%type <expr> test

%left "or"
%left "and"
%right "not"

%left "in" "is" "<" ">" "<=" ">=" "!=" "=="

%left "|"
%left "^"
%left "&"
%left "<<" ">>"
%left "+" "-"
%left "*" "@" "/" "//" "%"
%nonassoc UNARY_DUMMY
%left "**"

%%

/* let single_input :=
| NEWLINE; { Interactive [] }
| s=simple_stmt; { Interactive s }
| s=compound_stmt; NEWLINE; { Interactive [s] } */

let stmt_newline :=
| NEWLINE; { [] }
| s=stmt; { s }

let file_input :=
| s=stmt_newline*; ENDMARKER;
  { List.flatten s }

/* let eval_input :=
| y=testlist; NEWLINE*; ENDMARKER; { y } */

/* let decorator :=
| "@"; n=dotted_name; a=option("("; b=option(arglist); ")"); NEWLINE;
  { match a with
    | None -> Decorator (n, None)
    | Some _ ->
        match b with
        | None -> Decorator (n, [])
        | Some args -> Decorator (n, args) }

let decorators :=
| l=decorator+; { l }

let cfa :=
| f=funcdef; { fun d -> f d }
| a=async_funcdef; { fun d -> a d }

let decorated :=
| d=decorators; c=cfa; { c d } */

let async_funcdef :=
| ASYNC; f=funcdef; { let FunctionDef y = f in AsyncFunctionDef y }

let funcdef :=
| "def"; n=NAME; p=parameters; ":"; b=func_body_suite;
  { FunctionDef (n, p, b, [], None, None) }

let parameters :=
| "("; x=typedargslist?; ")";
  { match x with
    | None -> []
    | Some s -> s }

let typedargslist :=
| n=NAME+; { n }

/* let dotted_name :=
| n=separated_nonempty_list(".", NAME); { n } */

let compound_stmt :=
| i=if_stmt; { i }
| w=while_stmt; { w }
| f=for_stmt; { f }
| f=funcdef; { f }

let while_stmt :=
| "while"; cond=test; ":"; body=suite; { While (cond, body, []) }

let func_body_suite :=
| s=simple_stmt; { s }
| NEWLINE; INDENT; s=stmt+; DEDENT; { List.flatten s }

let stmt :=
| s=simple_stmt; { s }
| s=compound_stmt; { [s] }

let arglist :=
| l=separated_nonempty_list(",", argument); ","?; { l }

let argument :=
| t=test; { t }
| t1=test; ":="; t2=test; { NamedExpr (t1, t2) }

let comp_op :=
| "<"; { Lt }
| ">"; { Gt }
| "=="; { Eq}
| ">="; { GtE }
| "<="; { LtE }
| "<>"; { NotEq }
| "!="; { NotEq }
| "in"; { In }
| "not"; "in"; { NotIn } %prec IN
| "is"; { Is }
| "is"; "not"; { IsNot } %prec IN

let testlist :=
| l=separated_nonempty_list(",", test);
  { match l with
    | [h] -> h
    | [] -> failwith "impossible"
    | _ -> Tuple l }

let simple_stmt :=
| l=separated_nonempty_list(";", small_stmt); ";"?; NEWLINE; { l }

let small_stmt :=
| e=expr_stmt; { e }
| "pass"; { Pass }
| "break"; { Break }
| "continue"; { Continue }
| "return"; e=expr?; { Return e }

let or2(X, Y) :=
| x=X; { x }
| y=Y; { y }

/* let expr_stmt_helper :=
| a=annassign; { a } */
/* | a=augassign; or2(yield_expr, testlist); {} */
/* | option(nonempty_list("="; or2(yield_expr, testlist_star_expr))); {} */

let expr_stmt :=
| e1=testlist_star_expr; e3=option("="; e2=testlist_star_expr; { e2 });
  { match e3 with
    | None -> Expr e1
    | Some e -> Assign ([e1], e) }

/* let augassign :=
| "+="; {}
| "-="; {}
| "*="; {}
| "@="; {}
| "/="; {}
| "%="; {}
| "&="; {}
| "|="; {}
| "^="; {}
| "<<="; {}
| ">>="; {}
| "**="; {}
| "//="; {} */

let suite :=
| s=simple_stmt; { s }
| NEWLINE; INDENT; s=stmt+; DEDENT; { List.flatten s }

let yield_expr :=
| "yield"; option(yield_arg); {}

let yield_arg :=
| "from"; test; {}
| testlist_star_expr; {}

let testlist_star_expr :=
| t=test; { t }

let star_expr :=
| "*"; e=expr; { Starred e }

let operator :=
| "|"; { BitOr }
| "^"; { BitXor }
| "&"; { BitAnd }
| "<<"; { LShift }
| ">>"; { RShift }
| "+"; { Add }
| "-"; { Sub }
| "*"; { Mult }
| "/"; { Div }
| "//"; { FloorDiv }
| "@"; { MatMult }
| "%"; { Mod }

let factor_prefix :=
| "+"; { UAdd }
| "-"; { USub }
| "~"; { Invert }

let separated_least_two_list_and :=
| y1=expr; "and"; y2=expr; { [y1; y2] }
| y1=expr; "and"; y2=separated_least_two_list_and; { y1 :: y2 }

let separated_least_two_list_or :=
| y1=expr; "or"; y2=expr; { [y1; y2] }
| y1=expr; "or"; y2=separated_least_two_list_or; { y1 :: y2 }

let expr :=
| e1=expr; o=operator; e2=expr; { BinOp (e1, o, e2) }
| p=factor_prefix; e=expr; { UnaryOp (p, e) } %prec UNARY_DUMMY
| a=expr; "**"; e=expr; { BinOp (a, Pow, e) }
| l=separated_least_two_list_and; { BoolOp (And, l) }
| l=separated_least_two_list_or; { BoolOp (Or, l) }
| "not"; e=expr; { UnaryOp (Not, e) }
| e=atom; t=trailer*;
  { let initial = e in
    List.fold_left (fun acc i ->
      match i with
      | Call (_, args, kwargs) -> Call (acc, args, kwargs)
      | Subscript (_, c) -> Subscript (acc, c)
      | Attribute (_, x) -> Attribute (acc, x)
      | _ -> failwith "impossible") initial t }

let test :=
| e1=expr; l=list(c=comp_op; e2=expr; { c, e2 });
  { match l with
    | [] -> e1
    | _ ->
        let a, b = List.split l in
        Compare (e1, a, b) }

let trailer :=
| "("; b=option(arglist); ")";
  { match b with
    | None -> Call (Set [], [], [])
    | Some s -> Call (Set [], s, []) }
| "["; c=subscriptlist; "]";
  { Subscript (Set [], c) }
| "."; x=NAME;
  { Attribute (Set [], x) }

let atom :=
| s=NAME; { Name s }
| s=NUMBER; { Constant (Int s) }
| s=STRING+; { Constant (String (String.concat "" s)) }
| "..."; { Constant Ellipsis }
| "None"; { Constant NoneType }
| "True"; { Constant (Bool true) }
| "False"; { Constant (Bool false) }

let subscriptlist :=
| t=test; { Index t }

/* let atom_expr :=
| atom; trailer*; {} */

/* let annassign :=
| ":"; test; {} */

let for_stmt :=
| "for"; e1=exprlist; "in"; e2=testlist; ":"; s=suite;
  { For (e1, e2, s, [], None) }

let if_stmt :=
| "if"; e=test; ":"; s=suite;
  elifs=list("elif"; x=test; ":"; s2=suite;
    { If (x, s2, []) });
  elsepart=option("else"; ":"; suite);
  { let end_else =
      match elsepart with
      | None -> []
      | Some s -> s in
    let initial_if = If (Set [], [], end_else) in
    let elifs_folded = List.fold_right (fun if_stmt acc ->
      match acc, if_stmt with
      | If (_, _, prev_else), If (if_expr, if_body, _) -> If (Set [], [], [If (if_expr, if_body, prev_else)])
      | _ -> failwith "can't get here") elifs initial_if in
    match elifs_folded with
    | If (_, _, final_else) -> If (e, s, final_else)
    | _ -> failwith "impossible" }

let exprlist :=
| l=separated_nonempty_list(",", expr);
  { match l with
    | [] -> failwith "impossible"
    | [h] -> h
    | _ -> Tuple l }

/* let eval_input :=
| testlist; NEWLINE*; ENDMARKER; { None } */
