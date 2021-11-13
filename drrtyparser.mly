/* Ocamlyacc parser for DRRTY */

%{
open Ast
%}

/* A simple LR parser in OcamlYacc implementing a minimal Java/C syntax with some
limitations on exceptions, generators, importing modules, and some other features. */

%token SEMICOLON LPAREN RPAREN LBRACE RBRACE LKITE RKITE LBRACKET RBRACKET COMMA PLUS MINUS TIMES DIVIDE ASSIGN
%token /*EOL*/ EQ NOT NEQ LT LEQ GT GEQ AND OR 
%token RETURN INT BOOL /*RENDER IF ELSEIF ELSE FOR WHILE LIST DICTIONARY*/ VOID STRING
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID /*STRING_LITERAL*/
%token EOF

/* this tokenize target is simply used to take the input lexbuf stream and produce
a list of Parser.tokens for use in drrty.ml */

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

/* program: the main program parser target. read a list of statements until EOF is reached.
constructed backwards per the usual OCaml functional list syntax. */

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   FUNCTION typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $2;
	 fname = $3;
	 formals = List.rev $5;
	 locals = List.rev $8;
	 body = List.rev $9 } }

/* formals_opt: an optional formal name in a function declaration */

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }
//  | formal_dict   { $1 }

/* formal_list: a list of optional formal names in a function declaration */

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }


// dict_element:
//   typ ID                         { ($1:$2)}
//   | dict_element COLON typ ID    { $3, $4}

// formal_dict:
//   dict_element                      { {$1}     }
//   | formal_dict COMMA dict_element  { ($3,$4) :: $1 }
  
/* typ: the possible type attributes that can be attached to an optionally typed variable. */

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | VOID { Void }
  | STRING { String }
  // | LIST LBRACKET typ RBRACKET { List($3) }
  // | DICTIONARY LBRACE typ RBRACE { Dict($3) }

// vdecl_list:
//     /* nothing */    { [] }
//   | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMICOLON { ($1, $2) }

/* stmt_list: a list of statements in the global scope or a function body. modified from Micro C. */

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* stmt: this defines all possible statements in the DRRTY language. Those possible statements are:

a) an expression
b) another statement (in case of unusual behavior in the parser)
c) a class with a name and block of statements
d) a function with optional typed arguments
e) a function with explicit return type
f) a for loop
g) a while loop
e) a list of names or other valid lvalue expressions. will be expanded as more lvalues are supported
f) hard-coded type statements
g) hard-coded print statements
h) no operation statements

Other statements can be added by defining the appropriate syntax, and adding a new class of statements
to the ast.ml stmt type.
*/

stmt:
    expr SEMICOLON                               { Expr $1               }
  | RETURN expr_opt SEMICOLON                    { Return $2             }
  // | RENDER expr_opt SEMICOLON                    { Render $2             }
  | LBRACE stmt_list RBRACE                     { Block(List.rev $2)    }
  // | IF LPAREN expr RPAREN stmt %prec NOOTHERWISE { If($3, $5, Block([])) }
  // | IF LPAREN expr RPAREN stmt ELSE { If($3, $5, $7)        }
  // | FOR LPAREN expr_opt SEMICOLON expr SEMICOLON expr_opt RPAREN stmt
  //                                           { For($3, $5, $7, $9)   }
  // | WHILE LPAREN expr RPAREN stmt           { Repeat($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

/* expr: these are all possible expressions allowed in the DRRTY language. Each
expression corresponds to an expr object in the ast.ml file. Expressions are anything
that return a value, i.e. can be assigned to a variable. These include lists, list access
and list slice, methods and fields, function calls, binary and unary operations, and literals. */

expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  // | STRING_LITERAL   { StringLit($1) }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | LKITE expr RKITE { $2                   }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
