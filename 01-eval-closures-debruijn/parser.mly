%{
    open Lang
    open Term
%}

%token LET EQ IN
%token LAMBDA DOT
%token LPAR RPAR
%token<int> IDENT
%token EOF

%start main
%type<Lang.Term.t> main
%%

main:
   | def EOF { $1 }

def:
    | LET term IN def { Let ($2,$4) }
    | term { $1 }

term:
    | LAMBDA term { Abs $2 }
    | simple_term term { App ($1, $2) }
    | simple_term { $1 }

simple_term:
    | IDENT { Var $1 }
    | LPAR term RPAR { $2 }
