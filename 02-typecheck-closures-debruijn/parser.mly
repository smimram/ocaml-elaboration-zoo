%{
    open Lang
    open RawTerm
%}

%token LET EQ IN
%token LAMBDA DOT APP PI COLON TO U
%token LPAR RPAR
%token<string> IDENT
%token EOF

%nonassoc LAMBDA DOT
%nonassoc IDENT LPAR U
%right TO
%nonassoc APP

%start main
%type<Lang.RawTerm.t> main
%%

main:
   | def EOF { $1 }

def:
    | LET IDENT COLON term EQ term IN def { Let ($2, $4, $6, $8) }
    | term { $1 }

term:
    | LAMBDA vars DOT term { abs $2 $4 }
    | term term %prec APP { App ($1, $2) }
    | LPAR vars COLON term RPAR TO term { pi $2 $4 $7 }
    | term TO term { Pi ("_", $1, $3) }
    | U { U }
    | IDENT { Var $1 }
    | LPAR term RPAR { $2 }

vars:
    | IDENT vars { $1 :: $2 }
    | IDENT { [$1] }
