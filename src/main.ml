
(* Parser wrapper *)
let parse parser lex =
  try
    parser Lexer.token lex
  with
  | Parser.Error ->
      Error.syntax ~loc:(Location.of_lexeme lex) ""
  | Failure "lexing: empty token" ->
      Error.syntax ~loc:(Location.of_lexeme lex) "unrecognised symbol."

(* Interactive toplevel *)
let toplevel () =
  print_endline "ecaml...";
  try
    while true do
      try
        Format.printf  "@.# ";
        let cmd = Lexer.read_toplevel (parse Parser.commandline) () in
        let result = Eval.eval Map.empty cmd in
          match result with
          | Value.Value v ->
              Format.printf " = %t" (Value.print v)
          | Value.Perform(eff, _, _) ->
              Format.printf "Unhandled effect: %s" (eff :> string)
      with
        | Error.Error err -> Error.print err
        | Sys.Break -> prerr_endline "Interrupted."
    done
  with End_of_file -> ()

(* Main program *)
let main =
  Sys.catch_break true;
  toplevel ()
