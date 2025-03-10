{
open Lexing
open Parser

let advance_pos n lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }

(** Number of utf8 characters. *)
let utf8_length s =
  if not (String.is_valid_utf_8 s) then String.length s else
    let rec aux n i =
      if i >= String.length s then n else
        let c = String.get_utf_8_uchar s i in
        aux (n + 1) (i + Uchar.utf_decode_length c)
    in
    aux 0 0

(** Correct position for strings with utf8 characters. *)
let utf8 s lexbuf = advance_pos (String.length s - utf8_length s) lexbuf
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "λ" as s { utf8 s lexbuf; LAMBDA }
  | '\\' { LAMBDA }
  | "let" { LET }
  | "in" { IN }
  | ";" { IN }
  | "." { DOT }
  | "=" { EQ }
  | ":" { COLON }
  | "→" as s { utf8 s lexbuf; TO }
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
