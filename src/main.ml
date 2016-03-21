(* Main executable *)

(* Parser wrapper *)
let parse parser lex =
  try
    parser Lexer.token lex
  with
  | Parser.Error ->
      Error.syntax ~loc:(Location.of_lexeme lex) ""
  | Failure "lexing: empty token" ->
      Error.syntax ~loc:(Location.of_lexeme lex) "Unrecognised symbol."

(* Interactive toplevel *)
let toplevel () =
  Format.printf "    ______ ______                   __@.";
  Format.printf "   / ____// ____/____ _ ____ ___   / /@.";
  Format.printf "  / __/  / /    / __ `// __ `__ \\ / /@.";
  Format.printf " / /___ / /___ / /_/ // / / / / // /@.";
  Format.printf "/_____/ \\____/ \\__,_//_/ /_/ /_//_/@.";
  Format.printf "@.";
  try
    while true do
      try
        let cmd = Lexer.read_toplevel (parse Parser.commandline) () in
        let ty = Typecheck.infer Typecheck.empty cmd in
        let result = Eval.eval Map.empty cmd in
          match result with
          | Value.Value v ->
              Format.printf "- : %a = %t@." Typecheck.print ty (Value.print v)
          | Value.Perform(eff, _, _) ->
              Format.printf "Unhandled effect: %s@." (eff :> string)
      with
        | Error.Error err -> Error.print err
        | Sys.Break -> prerr_endline "Interrupted."
    done
  with End_of_file -> Format.printf "Goodbye!@."

(* Main program *)
let main =
  Sys.catch_break true;
  toplevel ()
