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
  | "(" { LPAR }
  | ")" { RPAR }
  | "_" { HOLE }
  | "U" { U }
  | (['A'-'Z''a'-'z']+ as s) { IDENT s }
  | "--"[^'\n']* { token lexbuf }
  | space+ { token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | eof { EOF }
