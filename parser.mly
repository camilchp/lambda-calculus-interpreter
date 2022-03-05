%{
  let rec list_to_assign (l:Lib.Lambda_exp.value list) (e:Lib.Lambda_exp.exp) =
    match l with
    | [] -> e
    | t::q -> Lib.Lambda_exp.Assign(t,list_to_assign q e)
%}

%token LAMBDA
%token <string> VAR
%token DOT
%token COLON
%token LPAR RPAR
%token EOL

%start main
%type <Lib.Lambda_exp.exp option> main

%%

main:
  | term; EOL {Some($1)}
  | EOL { None }
;

term:
  | VAR; COLON; term           { Lib.Lambda_exp.Alias($1, $3)           }
  | app_term                   { $1                                     }
  | LAMBDA; var_list; term     { list_to_assign (List.rev $2) $3                   }
;

app_term:
  | app_term; atom             { Lib.Lambda_exp.Eval($1,$2)             }
  | atom                       { $1                                     }

atom:
  | VAR                        { Lib.Lambda_exp.Val(Var($1))            }
  | LPAR; term; RPAR           { $2                                     }

var_list:
  | VAR; DOT                   { [Lib.Lambda_exp.Var($1)]               }
  | var_list; VAR; DOT;        { Lib.Lambda_exp.Var($2)::$1             }