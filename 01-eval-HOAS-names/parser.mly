%{
    open Lang
    open Term
%}

%token LET EQ IN
%token LAMBDA DOT
%token LPAR RPAR
%token<string> IDENT
%token EOF

%start main
%type<Lang.Term.t> main
%%

main:
   | def EOF { $1 }

def:
    | LET IDENT EQ term IN def { Let ($2,$4,$6) }
    | term { $1 }

term:
    | LAMBDA vars DOT term { abs $2 $4 }
    | term simple_term { App ($1, $2) }
    | simple_term { $1 }

simple_term:
    | IDENT { Var $1 }
    | LPAR term RPAR { $2 }

vars:
    | IDENT vars { $1 :: $2 }
    | IDENT { [$1] }
