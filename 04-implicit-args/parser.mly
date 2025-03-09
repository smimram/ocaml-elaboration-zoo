%{
open Preterm

let defpos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

let mk ?pos t =
  let pos = match pos with Some pos -> pos | None -> defpos () in
  mk ~pos t
%}

%token LET EQ IN
%token LAMBDA DOT APP COLON TO U HOLE
%token LPAR RPAR LACC RACC
%token<string> IDENT
%token EOF

%nonassoc LAMBDA DOT
%right TO
%nonassoc IDENT LPAR U HOLE
%nonassoc APP

%start main
%type<Preterm.t> main
%%

main:
   | def EOF { $1 }

def:
    | LET IDENT COLON term EQ term IN def { mk (Let ($2,$4,$6,$8)) }
    | term { $1 }

term:
    | LAMBDA vars DOT term { abss $2 $4 }
    | term term %prec APP { mk (App ($1,(`Explicit,$2))) }
    | IDENT { mk (Var $1) }
    | LPAR IDENT COLON term RPAR TO term { mk (Pi (($2,`Explicit,Some $4),$7)) }
    | LACC IDENT COLON term RACC TO term { mk (Pi (($2,`Implicit,Some $4),$7)) }
    | term TO term { arr ~pos:(defpos()) $1 $3 }
    | U { mk Type }
    | LPAR term RPAR { $2 }
    | HOLE { mk Hole }

vars:
    | IDENT vars { ($1, `Explicit, None) :: $2 }
    | { [] }
