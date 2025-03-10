open Extlib

let () =
  Printexc.record_backtrace true;
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  let t =
    try
      Parser.main Lexer.token lexbuf
    with
    | Failure err ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      failwith
        "Lexing error at line %d, character %d: %s"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        err
    | Parsing.Parse_error ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      failwith
        "Parse error at word \"%s\", line %d, character %d."
        (Lexing.lexeme lexbuf)
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
  in
  close_in ic;
  try
  Printf.printf "Parsing... done:\n%s\n\n%!" (Preterm.to_string t);
  Printf.printf "Typechecking... \n%!";
  let t, a = Lang.infer Lang.Context.empty t in
  let mv = String.concat "\n" @@ List.mapi (fun i t -> Printf.sprintf "?%d = %s" i (Option.fold ~none:"?" ~some:Value.to_string t.Value.value)) !Value.metavariables in
  Printf.printf "done: %s\n%s\n%s\n\n%!" (Value.to_string a) mv (Term.to_string t);
  Printf.printf "Normalizing... \n%!";
  let t = Value.normalize [] t in
  Printf.printf "done:\n%s\n%!" (Term.to_string t)
  with
  | Lang.Type_error(pos, e) ->
    Printf.eprintf "Typing error at %s:\n%s\n%!" (Pos.to_string pos) e;
    exit 1
