{
open Lexing
open Parser

let utf8 ?(n=1) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "λ" { utf8 lexbuf; LAMBDA }
  | '\\' { LAMBDA }
  | "let" { LET }
  | "in" { IN }
  | ";" { IN }
  | "." { DOT }
  | "=" { EQ }
  | ":" { COLON }
  | "->" { TO }
  | "{-" { comment 0 lexbuf; token lexbuf }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LACC }
  | "}" { RACC }
  | "_" { HOLE }
  | "U" { U }
  | (['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']* as s) { IDENT s }
  | "--"[^'\n']* { token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }

and comment level = parse
  | "{-" { comment (level+1) lexbuf }
  | "-}" { if level > 0 then comment (level-1) lexbuf }
  | "\n" { new_line lexbuf; comment level lexbuf }
  | _ { comment level lexbuf }
