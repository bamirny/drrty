(* Abstract Syntax Tree *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Float | Void | String | Html | List of typ

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
  | Seq of expr list (*ADDED SEQ*)
  | Html of string
  | Noexpr

type html = 
    Hexpr of expr (*will turn into strings *)
  |   Bitag of string * html 
  |   Tag of string   (*branches *)
  |   Hstringlit of string (* leaf nodes *)
  |   Hseq of html * html  (*branches *)

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

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
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Seq(s) -> string_of_list s (*ADDED SEQ*)
  (* | HSeq(s) -> string_of_list s  *)
  | Html(s) -> s (*should output html*)
  | Noexpr -> ""
  and string_of_list = function
      l -> "[" ^ (string_of_seq l) ^ "]"
  and string_of_seq = function
      x :: y :: a -> string_of_expr x ^ ", " ^ string_of_seq (y :: a)
    | x :: _ -> string_of_expr x
    | [] -> ""

(* let rec string_of_html = function
     Hexpr(e) -> string_of_expr e
  |  Bitag(t, h) -> t ^ string_of_html h ^ t
  |  Hstringlit(s) -> s
  |  Hseq(h1, h2) -> string_of_html h1 ^ string_of_html h2 ^ string_of_html h2 ^ string_of_html h1
  and string_of_hseq = function
    x :: y :: a -> string_of_expr x ^ ", " ^ string_of_hseq (y :: a)
  | x :: _ -> string_of_expr x
  | [] -> "" *)

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

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "str"
  | List(t) -> "List(" ^ string_of_typ t ^ ")"
  | Html -> "html"  (* will need to change on how we want string to look *)

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "function " ^ string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n:\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "\n" ^ "end"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
