(* Semantically-checked Abstract Syntax Tree *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SBoolLit of bool
  | SId of string
  | SStringLit of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SListGet of typ * string * sexpr
  | SListPop of typ * string
  | SListLength of typ * string
  | SListSlice of typ * string * sexpr * sexpr
  | SListIndex of typ * string * sexpr
  | SListLit of typ * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SListAppend of string * sexpr
  | SListSet of typ * string * sexpr * sexpr
  | SListClear of typ * string
  | SListRemove of string * sexpr
  | SListInsert of string * sexpr * sexpr
  | SListReverse of typ * string

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "True"
  | SBoolLit(false) -> "False"
  | SFliteral(l) -> l
  | SStringLit(s) -> s
  | SListGet(_, id, e) -> "get " ^ id ^ ", " ^ (string_of_sexpr e)
  | SListPop (_, id) -> "pop " ^ id
  | SListLength(_, id) -> "length " ^ id
  | SListSlice(_, id, e1, e2) -> "slice " ^ id ^ ", " ^ (string_of_sexpr e1) ^ ", " ^ (string_of_sexpr e2)
  | SListIndex(_, id, e) -> "index " ^ id ^ ", " ^ (string_of_sexpr e)
  | SListLit(_) -> "list literal"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
				  ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "Return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SListAppend(id, e) -> "append " ^ id ^ ", " ^ string_of_sexpr e
  | SListSet(_, id, e1, e2) -> "set " ^ id ^ ", " ^ (string_of_sexpr e1) ^ ", " ^ (string_of_sexpr e2)
  | SListClear(_, id) -> "clear " ^ id
  | SListRemove(id, e) -> "remove " ^ id ^ ", " ^ (string_of_sexpr e)
  | SListInsert(id, e1, e2) -> "insert " ^ id ^ ", " ^ (string_of_sexpr e1) ^ ", " ^ (string_of_sexpr e2)
  | SListReverse(_, id) -> "reverse " ^ id

let string_of_sfdecl fdecl =
  "def " ^ string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n:\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
