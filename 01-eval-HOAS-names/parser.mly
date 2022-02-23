%{
    open Lang
    open Term
%}

%token LET EQ IN
%token LAMBDA DOT APP
%token LPAR RPAR
%token<string> IDENT
%token EOF

%nonassoc LAMBDA DOT
%nonassoc IDENT LPAR
%nonassoc APP

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
    | term term %prec APP { App ($1, $2) }
    | IDENT { Var $1 }
    | LPAR term RPAR { $2 }

vars:
    | IDENT vars { $1 :: $2 }
    | IDENT { [$1] }
