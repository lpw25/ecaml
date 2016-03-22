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

type env = {
  typecheck: Typecheck.env;
  eval: (Syntax.variable, Value.value) Map.t
}

let initial = {
  typecheck = Typecheck.empty;
  eval = Map.empty
}

(* Interactive toplevel *)
let run_command env = function
  | Syntax.Term t ->
      let ty = Typecheck.infer env.typecheck t in
      let result = Eval.eval env.eval t in
        begin match result with
        | Value.Value v ->
            Format.printf "- : %a = %t@." Typecheck.print ty (Value.print v)
        | Value.Perform(eff, _, _) ->
            Format.printf "Unhandled effect: %s@." (eff :> string)
        end;
      env
  | Syntax.Let (pat, t) ->
      let ty = Typecheck.infer env.typecheck t in
      let result = Eval.eval env.eval t in
        begin match result with
        | Value.Value v ->
            Format.printf "- : %a = %t@." Typecheck.print ty (Value.print v);
            {
              typecheck = Typecheck.extend_poly_env ~loc:t.location env.typecheck ty pat;
              eval = Eval.extend env.eval pat v
            }
        | Value.Perform(eff, _, _) ->
            Format.printf "Unhandled effect: %s@." (eff :> string);
            env
        end

let rec toplevel env =
    let env = try
      let cmd = Lexer.read_toplevel (parse Parser.commandline) () in
      run_command env cmd
    with
      | Error.Error err -> Error.print err; env
      | Sys.Break -> prerr_endline "Interrupted."; env
    in
    toplevel env

(* Main program *)
let main =
  Sys.catch_break true;
  Format.printf "    ______ ______                   __@.";
  Format.printf "   / ____// ____/____ _ ____ ___   / /@.";
  Format.printf "  / __/  / /    / __ `// __ `__ \\ / /@.";
  Format.printf " / /___ / /___ / /_/ // / / / / // /@.";
  Format.printf "/_____/ \\____/ \\__,_//_/ /_/ /_//_/@.";
  Format.printf "@.";
  try
    toplevel initial
  with
    | End_of_file -> Format.printf "Goodbye!@."
