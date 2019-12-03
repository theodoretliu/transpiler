{
  open Lexing
  open PythonParser

  let reset_lexbuf (filename:string) (lnum:int) lexbuf : unit =
    lexbuf.lex_curr_p <- {
      pos_fname = filename;
      pos_cnum = 0;
      pos_bol = 0;
      pos_lnum = lnum;
    }

  let newline lexbuf =
    lexbuf.lex_curr_p <- { (lexeme_end_p lexbuf) with
      pos_lnum = (lexeme_end_p lexbuf).pos_lnum + 1;
      pos_bol = (lexeme_end lexbuf) }

  (* Boilerplate to define exceptional cases in the lexer. *)
  let unexpected_char lexbuf (c:char) : 'a =
    failwith "Unexpected character"

  (* Lexing reserved words *)
  let reserved_words = [
    (* Keywords *)
    "def", DEF ;
    "while", WHILE ;
    "or", OR ;
    "and", AND ;
    "not", NOT ;
    "in", IN ;
    "is", IS ;
    "yield", YIELD ;
    "from", FROM ;
    "None", NONE ;
    "True", TRUE ;
    "False", FALSE ;
    "for", FOR ;
    "else", ELSE ;
    "elif", ELIF ;
    "if", IF ;
    "async", ASYNC ;
    "pass", PASS ;
    "break", BREAK ;
    "continue", CONTINUE ;
    "return", RETURN ;

    (* Symbols *)
    "@", AT ;
    "(", LPAREN ;
    ")", RPAREN ;
    ":", COLON ;
    "->", RARROW ;
    ".", DOT ;
    ",", COMMA ;
    ":=", COLONEQUAL ;
    "=", EQUAL ;
    "**", DOUBLESTAR ;
    "*", STAR ;
    "<", LESS ;
    ">", GREATER ;
    "==", EQEQUAL ;
    "<=", LESSEQUAL ;
    ">=", GREATEREQUAL ;
    "<>", LESSGREATER ;
    "!=", NOTEQUAL ;
    ";", SEMICOLON ;
    "+=", PLUSEQUAL ;
    "-=", MINUSEQUAL ;
    "*=", STAREQUAL ;
    "@=", ATEQUAL ;
    "/=", SLASHEQUAL ;
    "%=", PERCENTEQUAL ;
    "&=", ANDEQUAL ;
    "|=", VBAREQUAL ;
    "^=", CIRCUMFLEXEQUAL ;
    "<<=", LEFTSHIFTEQUAL ;
    ">>=", RIGHTSHIFTEQUAL ;
    "**=", DOUBLESTAREQUAL ;
    "//", DOUBLESLASH ;
    "//=", DOUBLESLASHEQUAL ;
    "|", VBAR ;
    "^", CIRCUMFLEX ;
    "&", AMPER ;
    "<<", LEFTSHIFT ;
    ">>", RIGHTSHIFT ;
    "+", PLUS ;
    "-", MINUS ;
    "/", SLASH ;
    "%", PERCENT ;
    "~", TILDE ;
    "[", LSQB ;
    "]", RSQB ;
    "{", LBRACE ;
    "}", RBRACE ;
    "...", ELLIPSIS ;
  ]

  let symbol_table : (string, PythonParser.token) Hashtbl.t = Hashtbl.create 1024
  let _ =
    List.iter (fun (str,t) -> Hashtbl.add symbol_table str t) reserved_words

  let create_token lexbuf =
    let str = lexeme lexbuf in
    try (Hashtbl.find symbol_table str)
    with _ -> NAME str

  (* Lexing comments and strings *)
  let string_buffer = ref (Bytes.create 2048)
  let string_end = ref 0


  let add_str ch =
    let x = !string_end in
    let buffer = !string_buffer
    in
      if x = Bytes.length buffer then
        begin
          let new_buffer = Bytes.create (x*2) in
          Bytes.blit buffer 0 new_buffer 0 x;
          Bytes.set new_buffer x ch;
          string_buffer := new_buffer;
          string_end := x+1
        end
      else
        begin
          Bytes.set buffer x ch;
          string_end := x+1
        end

  let get_str () = Bytes.sub_string (!string_buffer) 0 (!string_end)

  (* Lexing directives *)
  let lnum = ref 1
}

(* Declare your aliases (let foo = regex) and rules here. *)
let newline = '\n'
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let character = uppercase | lowercase
let whitespace = ['\t' ' ']
let leading_whitespace = newline whitespace*
let digit = ['0'-'9']
let hexdigit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']

rule token = parse
| eof { ENDMARKER }
| character (digit | character | '_')* { create_token lexbuf }
| digit+ { NUMBER (lexeme lexbuf) }
| '\n'+ { new_line lexbuf; NEWLINE }
| whitespace+
  { let distance = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    if distance = String.length (lexeme lexbuf)
    then SPACES (String.length (lexeme lexbuf))
    else token lexbuf }
| '@' | '(' | ')' | ':' | "->" | '.' | ',' | ":=" | '=' | "**" | '*'
| '<' | '>' | "==" | "<=" | ">=" | "<>" | "!=" | ';' | "+=" | "-="
| "*=" | "@=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**="
| "//" | "//=" | '|' | '^' | '&' | "<<" | ">>" | '+' | '-' | '/' | '%'
| '~' | '[' | ']' | '{' | '}' | "..." { create_token lexbuf }
| _ as c { unexpected_char lexbuf c }
