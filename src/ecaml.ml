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
      let ty_and_dirt = Typecheck.infer env.typecheck t in
      let result = Eval.eval env.eval t in
        begin match result with
        | Value.Value v ->
            Format.printf "- : %a = %t@." 
              Typecheck.print_type_and_effect ty_and_dirt
              (Value.print v)
        | Value.Perform(eff, _, _) ->
            Format.printf "Unhandled effect: %s@." (eff :> string)
        end;
      env
  | Syntax.Let (pat, t) ->
      let (ty, dirt) as ty_and_dirt = Typecheck.infer env.typecheck t in
      let result = Eval.eval env.eval t in
        begin match result with
        | Value.Value v ->
            Format.printf "- : %a = %t@."
              Typecheck.print_type_and_effect ty_and_dirt
              (Value.print v);
            {
              typecheck = Typecheck.extend_poly_env ~loc:t.location env.typecheck ty pat;
              eval = Eval.extend env.eval pat v
            }
        | Value.Perform(eff, _, _) ->
            Format.printf "Unhandled effect: %s@." (eff :> string);
            env
        end

let run_file env filename =
  let cmds = Lexer.read_file (parse Parser.file) filename in
  List.fold_left run_command env cmds

let rec toplevel env =
    let env = try
      let cmd = Lexer.read_toplevel (parse Parser.commandline) () in
      run_command env cmd
    with
      | Error.Error err -> Error.print err; env
      | Sys.Break -> prerr_endline "Interrupted."; env
    in
    toplevel env

let options = Arg.align [
  ("-n",
    Arg.Clear Config.run_interactive_toplevel,
    " Do not run the interactive toplevel");
]

let files_to_load = ref []

let anonymous arg = files_to_load := arg :: !files_to_load

let usage = "ecaml.native [file] ..."

(* Main program *)
let main =
  Sys.catch_break true;
  Arg.parse options anonymous usage;
  let env = List.fold_left run_file initial (List.rev !files_to_load) in
  if !Config.run_interactive_toplevel then
  begin
    Format.printf "    ______ ______                   __@.";
    Format.printf "   / ____// ____/____ _ ____ ___   / /@.";
    Format.printf "  / __/  / /    / __ `// __ `__ \\ / /@.";
    Format.printf " / /___ / /___ / /_/ // / / / / // /@.";
    Format.printf "/_____/ \\____/ \\__,_//_/ /_/ /_//_/@.";
    Format.printf "@.";
    try
      toplevel env
    with
      | End_of_file -> Format.printf "Goodbye!@."
  end
