open Lang

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
      let err =
        Printf.sprintf
          "Lexing error at line %d, character %d: %s"
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
          err
      in
      failwith err
    | Parsing.Parse_error ->
      let pos = (Lexing.lexeme_end_p lexbuf) in
      let err =
        Printf.sprintf
          "Parse error at word \"%s\", line %d, character %d."
          (Lexing.lexeme lexbuf)
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      in
      failwith err
  in
  close_in ic;
  Printf.printf "Parsing... done:\n%s\n\n%!" (RawTerm.to_string t);
  Printf.printf "Typechecking... \n%!";
  let t, a = infer [] [] [] 0 t in
  let mv = List.map (fun (i,m) -> Term.to_string (Term.Meta i) ^ " = " ^ (match !(snd m) with None -> "?" | Some t -> to_string t)) !metavariables |> String.concat "\n" in
  Printf.printf "done: %s\n%s\n%s\n\n%!" (Term.to_string (quote 0 a)) mv (Term.to_string t);
  Printf.printf "Normalizing... \n%!";
  let t = normalize [] t in
  Printf.printf "done:\n%s\n%!" (Term.to_string t)
