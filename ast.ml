(* Abstract Syntax Tree *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Float | Void | String | List of typ

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | Id of string
  | StringLit of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | ListGet of string * expr
  | ListLength of string
  | ListPop of string
  | ListIndex of string * expr
  | ListSlice of string * expr * expr
  | ListLit of expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | ListAppend of string * expr
  | ListSet of string * expr * expr
  | ListClear of string
  | ListRemove of string * expr
  | ListInsert of string * expr * expr
  | ListReverse of string

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | StringLit s -> "\"" ^ s ^ "\""
  | ListGet(id, e) -> "get " ^ id ^ ", " ^ (string_of_expr e)
  | ListPop(id) -> "pop " ^ id
  | ListLength(id) -> "length " ^ id
  | ListSlice(id, e1, e2) -> "slice " ^ id ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2)
  | ListIndex(id, e) -> "index " ^ id ^ ", " ^ (string_of_expr e)
  | ListLit(_) -> "list literal"
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

  and string_of_list = function
      l -> "[" ^ (string_of_seq l) ^ "]"
  and string_of_seq = function
      x :: y :: a -> string_of_expr x ^ ", " ^ string_of_seq (y :: a)
    | x :: _ -> string_of_expr x
    | [] -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "Return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | ListAppend(id, e) -> "append " ^ id ^ ", " ^ string_of_expr e
  | ListSet(id, e1, e2) -> "set " ^ id ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2)
  | ListClear(id) -> "clear " ^ id
  | ListRemove(id, e) -> "remove " ^ id ^ ", " ^ (string_of_expr e)
  | ListInsert(id, e1, e2) -> "insert " ^ id ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2)
  | ListReverse(id) -> "reverse " ^ id

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "str"
  | List(t) -> "list[" ^ string_of_typ t ^ "]"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "def " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n:\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "\n" ^ "end"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
