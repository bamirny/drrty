(* Semantic checking for the DRRTY compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no none types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name;
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float);
                               ("prints", String);
                               ("makeHeader", String);
                               ("makeSubheader", String);
                               ("makeText", String);
                               ("makeImage", String);
                               ("makeList", String)]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are none or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let check_list_type id =
      match (type_of_identifier id) with
        List t -> t
      | t -> raise  (Failure ("invalid list type: " ^ string_of_typ t))

    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal  l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Noexpr     -> (Void, SNoexpr)
      | StringLit s -> (String, SStringLit s)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
              Add | Sub | Mult | Div when same && t1 = Int   -> Int
            | Add | Sub | Mult | Div when same && t1 = Float -> Float
            | Add                          when same && t1 = String -> String
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	          Failure ("illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | ListGet(var, e) -> 
        let (t, e') = expr e in
        let ty = match t with 
            Int -> Int
            | _ -> raise (Failure ("expected type int for index, not " ^ string_of_typ t)) 
        in let list_type = check_list_type var
        in (list_type, SListGet(list_type, var, (ty, e')))
      | ListPop var -> 
        let list_type = check_list_type var
        in (list_type, SListPop(list_type, var))
      | ListLength var -> 
          (Int, SListLength(check_list_type var, var))
      | ListSlice(var, e1, e2) ->
          let e1' = expr e1 
          and e2' = expr e2 in
          let _ = match (fst e1', fst e2') with
              (Int, Int) 
            | (Void, Int) 
            | (Int, Void)
            | (Void, Void) -> Int
            |  _ -> raise (Failure ("invalid indices for list slice: " ^ string_of_sexpr e1' ^ ", " ^ string_of_sexpr e2'))
          in (type_of_identifier var, SListSlice(check_list_type var, var, e1', e2'))
      | ListIndex(var, e) ->
        let (t, e') = expr e in
        (Int, SListIndex(check_list_type var, var, (t, e')))
      | ListLit vals ->
        let (t', _) = expr (List.hd vals) in
        let map_func lit = expr lit in
        let vals' = List.map map_func vals in (List t', SListLit(t', vals'))
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    let check_int_expr e = 
      let (t', e') = expr e
      and err = "expected Integer expression in " ^ string_of_expr e
      in if t' != Int then raise (Failure err) else (t', e')
      
    in

    let check_match_list_type_expr l e =
      let (t', e') as e'' = expr e
    in let err = "incompatible list and expression types: " ^ (string_of_typ t') ^ ", " ^ (string_of_sexpr e'') in
    if t' != (check_list_type l) then raise (Failure err) else (t', e') 
  
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | ListAppend(var, e) -> 
        let _ = check_list_type var in
        SListAppend(var, check_match_list_type_expr var e)
      | ListSet(var, e1, e2) ->
          SListSet(check_list_type var, var, check_int_expr e1, check_match_list_type_expr var e2)
      | ListClear var ->
          SListClear(check_list_type var, var)
      | ListRemove (var, e) ->
          let _ = check_list_type var in
          SListRemove(var, check_match_list_type_expr var e)
      | ListInsert (var, e1, e2) ->
          let _ = check_list_type var in
          SListInsert(var, check_int_expr e1, check_match_list_type_expr var e2)
      | ListReverse var -> 
          SListReverse(check_list_type var, var)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) -> SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e')
        else raise (
	        Failure ("output gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a output")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
