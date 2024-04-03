%{ open Ast %}

%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE COMMA SEMI 
%token PLUS MINUS TIMES DIV MOD ASSIGN EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE FOR WHILE CONTINUE BREAK
%token ARROW RETURN
%token NUM BOOL STRING FUNC NONE
%token EOF
%token <float> NUMLIT
%token <bool> BOOLLIT
%token <string> STRINGLIT
%token <string> ID

%right ASSIGN
%left LSQUARE
%left ARROW
%left PLUS MINUS
%left TIMES DIV MOD
%left EQ NEQ LT LEQ GT GEQ
%left AND
%left OR
%right NOT


%start program
%type <Ast.program> program

%%

program:
  stmt_list EOF { $1 }

stmt_list:
   /* nothing */ { ([], [])               }
 | stmt stmt_list { (($1 :: fst $2), snd $2) }
 | func_def stmt_list { (fst $2, ($1 :: snd $2)) }

vdecl:
  typ ID { ($1, $2) }

/* fdecl */
func_def:
  FUNC STRINGLIT LPAREN formals_opt RPAREN ARROW typ LCURLY stmt_list RCURLY
  {
      {
        rtyp=$7;
        fname=$2;
        formals=$4;
        body=$9
      }
    }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

typ:
    NUM   { Num   }
  | BOOL  { Bool  }
  | STRING { String }
  | typ LSQUARE RSQUARE {Arr ($1)} 
  | FUNC LPAREN typ_opt RPAREN ARROW typ { Func ($3, $6) }
  | LPAREN typ RPAREN { $2 }


typ_opt:
  /*nothing*/ { [] }
  | typ_list { $1 }

typ_list:
  typ { [$1] }
  | typ COMMA typ_list { $1::$3 }

stmt:
    expr SEMI                               { Expr $1      }
  | LCURLY stmt_list RCURLY                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt   { For ($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | CONTINUE SEMI                           {Continue }
  | BREAK SEMI                              {Break }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
    NUMLIT          { Literal($1)            }
  | BOOLLIT             { BoolLit($1)         }
  | STRINGLIT           { StringLit($1) }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Add,   $3)   }
  | expr DIV    expr { Binop($1, Sub,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Lt,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,  $3)   }
  | expr GT     expr { Binop($1, Gt,  $3)   }
  | expr GEQ    expr { Binop($1, Geq,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | NOT expr         { Unop(Not, $2)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }