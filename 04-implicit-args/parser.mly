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
  | LAMBDA idents DOT term { abss (List.map (fun x -> x,`Explicit,None) $2) $4 }
  | pi_args TO term { pis ~pos:(defpos()) $1 $3 }
  | term TO term { arr ~pos:(defpos()) $1 $3 }
  | aterm { $1 }

// Application term
aterm:
  | aterm sterm { mk (App ($1,(`Explicit,$2))) }
  | sterm { $1 }

// Simple term
sterm:
  | IDENT { mk (Var $1) }
  | U { mk Type }
  | LPAR term RPAR { $2 }
  | HOLE { mk Hole }

idents:
  | IDENT idents { $1 :: $2 }
  | IDENT { [$1] }

opt_type:
  | COLON term { Some $2 }
  | { None }

pi_arg:
  | LPAR idents opt_type RPAR { List.map (fun x -> x,`Explicit,$3) $2 }
  | LACC idents opt_type RACC { List.map (fun x -> x,`Implicit,$3) $2 }

pi_args:
  | pi_arg pi_args { $1@$2 }
  | pi_arg { $1 }

