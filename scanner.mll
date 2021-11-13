(* Ocamllex scanner for DRRTY *)

{ open drrtyparser }


let digit = ['0' - '9']
let digits = digit+

(* let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let string = '"' ( (ascii | escape)* as s) '"' *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Open comment *)
| '('      { LPAREN }                    (* Left paran *)
| ')'      { RPAREN }                    (* Right paran *)
| '{'      { LBRACE }                    (* Left brace *)
| '}'      { RBRACE }                    (* Right brace *)
| '['      { LBRACKET }                  (* Left bracket *)
| ']'      { RBRACKET }                  (* Right bracket *)
| "<>"     { LKITE }                     (* Opening Kite *)
| "</>"    { RKITE }                     (* Closing Kite *)
| ';'      { SEMICOLON }                 (* semicolons *)
| ','      { COMMA }                     (* commas *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
(* | '\n'     { EOL } *)
| "=="     { EQ }
| "!="     { NEQ }
| "<"      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "AND"    { AND }
| "OR"     { OR }
| "!"      { NOT }
| "def"    { FUNCTION }
(* | "if"     { IF }
| "elseif" { ELSEIF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE } *)
| "return" { RETURN }
(* | "render" { RENDER } *)
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "string" { STRING }
(* | "list"   { LIST }
| "dict"   { DICTIONARY } *)
| "True"   { BLIT(true)  }
| "False"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
(* | string   { STRING_LITERAL } *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) } (* Error checking to make sure this char is recognized by our lang *)

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
