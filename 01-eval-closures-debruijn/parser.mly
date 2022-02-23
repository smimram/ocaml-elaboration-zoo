%{
    open Lang
    open Term
%}

%token LET EQ IN
%token LAMBDA DOT APP
%token LPAR RPAR
%token<int> IDENT
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
    | LET term IN def { Let ($2,$4) }
    | term { $1 }

term:
    | LAMBDA term { Abs $2 }
    | term term %prec APP { App ($1, $2) }
    | IDENT { Var $1 }
    | LPAR term RPAR { $2 }
