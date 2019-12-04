type identifier = string
type constant =
| String of string
| Int of int
| NoneType
| Ellipsis
| Bool of bool


type prog = stmt list

and stmt =
| FunctionDef of identifier * arguments * stmt list * expr list * expr option * string option
| AsyncFunctionDef of identifier * arguments * stmt list * expr list * expr option * string option
| ClassDef of identifier * expr list * keyword list * stmt list * expr list
| Return of expr option

| Delete of expr list
| Assign of expr list * expr
| AugAssign of expr * operator * expr

| AnnAssign of expr * expr * expr option * bool

| For of expr * expr * stmt list * stmt list * string option
| AsyncFor of expr * expr * stmt list * stmt list * string option

| While of expr * stmt list * stmt list
| If of expr * stmt list * stmt list

| With of withitem list * stmt list * string option
| AsyncWith of withitem list * stmt list * string option

| Raise of expr option * expr option
| Try of stmt list * excepthandler list * stmt list * stmt list
| Assert of expr * expr option

| Import of alias list
| ImportFrom of identifier option * alias list * int option

| Global of identifier list
| Nonlocal of identifier list
| Expr of expr
| Pass
| Break
| Continue

(* (int lineno, int col_offset, int? end_lineno, int? end_col_offset) *)
and attributes = int * int * int option * int option

and expr =
| BoolOp of boolop * expr list
| NamedExpr of expr * expr
| BinOp of expr * operator * expr
| UnaryOp of unaryop * expr
| Lambda of arguments * expr
| IfExp of expr * expr * expr
| Dict of expr list * expr list
| Set of expr list
| ListComp of expr * comprehension list
| SetComp of expr * comprehension list
| DictComp of expr * expr * comprehension list
| GeneratorExp of expr * comprehension list
(* -- the grammar constrains where yield expressions can occur *)
| Await of expr
| Yield of expr option
| YieldFrom of expr

(* -- need sequences for compare to distinguish between *)
(* -- x < 4 < 3 and (x < 4) < 3 *)
| Compare of expr * cmpop list * expr list
| Call of expr * expr list * keyword list
| FormattedValue of expr * int option * expr option
| JoinedStr of expr list
| Constant of constant

(* -- the following expression can appear in assignment context *)
| Attribute of expr * identifier
| Subscript of expr * slice
| Starred of expr
| Name of identifier
| List of expr list
| Tuple of expr list

 (* -- col_offset is the byte offset in the utf8 string the parser uses
 attributes (int lineno, int col_offset, int? end_lineno, int? end_col_offset) *)

and expr_context = Load | Store | Del | AugLoad | AugStore | Param

and slice =
| Slice of expr option * expr option * expr option
| ExtSlice of slice list
| Index of expr

and boolop = And | Or

and operator = Add | Sub | Mult | MatMult | Div | Mod | Pow | LShift
        | RShift | BitOr | BitXor | BitAnd | FloorDiv

and unaryop = Invert | Not | UAdd | USub

and cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn

and comprehension = expr * expr * expr list * bool

and excepthandler =
| ExceptHandler of expr option * identifier option * stmt list

and arguments = arg list (* arg list * arg option * arg list * expr list * arg option * expr list *)

and arg = identifier (* expr option * string option *)

(* -- keyword arguments supplied to call (NULL identifier for **kwargs) *)
and keyword = identifier option * expr

(* -- import name with optional 'as' alias. *)
and alias = identifier * identifier option

and withitem = expr * expr option

and type_ignore =
| TypeIgnore of int * string
| Exp
