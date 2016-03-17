{
  open Parser

  let reserved = [
    ("effect", EFFECT);
    ("else", ELSE);
    ("false", BOOL false);
    ("fun", FUN);
    ("if", IF);
    ("in", IN);
    ("let", LET);
    ("match", MATCH);
    ("then", THEN);
    ("true", BOOL true);
    ("with", WITH)
  ]
  let reserved = Map.of_list reserved

  let escaped_characters = [
    ("\"", "\"");
    ("\\", "\\");
    ("\'", "'");
    ("n", "\n");
    ("t", "\t");
    ("b", "\b");
    ("r", "\r");
    (" ", " ");
  ]
}

let lname = ( ['a'-'z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9' '\'']*
            | ['_' 'a'-'z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9' '\'']+)

let uname = ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9' '\'']*

let hexdig = ['0'-'9' 'a'-'f' 'A'-'F']

let int = ['0'-'9'] ['0'-'9' '_']*

let xxxint =
    ( ("0x" | "0X") hexdig (hexdig | '_')*
    | ("0o" | "0O") ['0'-'7'] ['0'-'7' '_']*
    | ("0b" | "0B") ['0' '1'] ['0' '1' '_']*)

let float =
  '-'? ['0'-'9'] ['0'-'9' '_']* 
  (('.' ['0'-'9' '_']*) (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)? |
   ('.' ['0'-'9' '_']*)? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*))

let operatorchar = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '.' '<' '=' '>' '?' '@' '^' '|' '~']

let prefixop = ['~' '?' '!']             operatorchar*
let infixop0 = ['=' '<' '>' '|' '&' '$'] operatorchar*
let infixop1 = ['@' '^']                 operatorchar*
let infixop2 = ['+' '-']                 operatorchar*
let infixop3 = ['*' '/' '%']             operatorchar*
let infixop4 = "**"                      operatorchar*

rule token = parse
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\r' '\t']     { token lexbuf }
  | "(*"                { comment 0 lexbuf }
  | int                 { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | xxxint              { try
                            INT (int_of_string (Lexing.lexeme lexbuf))
                          with Failure _ -> Error.syntax ~loc:(Location.of_lexeme lexbuf) "Invalid integer constant"
                        }
  | '"'                 { STRING (string "" lexbuf) }
  | lname               { let s = Lexing.lexeme lexbuf in
                            match Map.lookup s reserved with
                              | Some t -> t
                              | None -> LNAME s
                        }
  | '\'' lname          { let str = Lexing.lexeme lexbuf in
                          PARAM (String.sub str 1 (String.length str - 1)) }
  | "\'~"               { PARAM ("~") }
  | '_'                 { UNDERSCORE }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ':'                 { COLON }
  | ','                 { COMMA }
  | '|'                 { BAR }
  | ";;"                { SEMISEMI }
  | ';'                 { SEMI }
  | "-["                { LARROW }
  | "]->"               { RARROW }
  | "->"                { ARROW }
  | "=>"                { PUREARROW }
  | "~>"                { TILDEARROW }
  | '='                 { EQUAL }
  | '*'                 { STAR }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | eof                 { EOF }

and comment n = parse
  | "*)"                { if n = 0 then token lexbuf else comment (n - 1) lexbuf }
  | "(*"                { comment (n + 1) lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comment n lexbuf }
  | _                   { comment n lexbuf }
  | eof                 { Error.syntax ~loc:(Location.of_lexeme lexbuf) "Unterminated comment" }

and string acc = parse
  | '"'                 { acc }
  | '\\'                { let esc = escaped lexbuf in string (acc ^ esc) lexbuf }
  | [^'"' '\\']*        { string (acc ^ (Lexing.lexeme lexbuf)) lexbuf }
  | eof                 { Error.syntax ~loc:(Location.of_lexeme lexbuf) "Unterminated string %s" acc}

and escaped = parse
  | _                   { let str = Lexing.lexeme lexbuf in
                          try List.assoc str escaped_characters
                          with Not_found -> Error.syntax ~loc:(Location.of_lexeme lexbuf) "Unknown escaped character %s" str
                        }

{
  let read_file parser fn =
  try
    let fh = open_in fn in
    let lex = Lexing.from_channel fh in
    lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
    try
      let terms = parser lex in
      close_in fh;
      terms
    with
      (* Close the file in case of any parsing errors. *)
      Error.Error err -> close_in fh; raise (Error.Error err)
  with
    (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> Error.fatal "%s" msg


  let read_toplevel parser () =

    let has_semisemi str =
      let in_quote = ref false in
      let last_backslash = ref false in
      let last_semi = ref false in
      let semisemi = ref false in
      let i = ref 0 in
      while !i < String.length str && not !semisemi do
        begin
          match str.[!i], !last_backslash, !in_quote, !last_semi with
            | '\\', b, _, _ -> last_backslash := not b; last_semi := false
            | '"', false, b, _ -> in_quote := not b; last_backslash := false; last_semi := false
            | ';', false, false, b -> semisemi := b; last_semi := true
            | _, _, _, _ -> last_backslash := false; last_semi := false
        end;
        incr i
      done;
      if !semisemi then
        Some (String.sub str 0 !i)
      else
        None
    in

    let rec read_more prompt acc =
      match has_semisemi acc with
      | Some acc -> acc
      | None ->
          print_string prompt;
          let str = read_line () in
          read_more "  " (acc ^ "\n" ^ str)
    in

    let str = read_more "# " "" in
    let lex = Lexing.from_string (str ^ "\n") in
    let cmd = parser lex in
    cmd
}
