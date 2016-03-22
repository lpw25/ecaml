%{
  open Location
  open Syntax

  type handler_case =
    | EffectCase of effect * abstraction2
    | ReturnCase of abstraction

  let collect_cases lst =
    List.fold_left
        (fun {effects; values} -> function
          | {plain = EffectCase (op, a2)} ->
              let effects = Map.update op a2 effects in
              {effects; values}
          | {plain = ReturnCase a} ->
              let values = a :: values in
                {effects; values})
        {effects = Map.empty; values = []}
        lst

  let io_dirt = { operations = [effect "IO"]; row = None }
  let tilde_dirt = { operations = [effect ""]; row = Some (rowparam "~") }
  let pure_dirt = { operations = []; row = None }

%}

%token LPAREN RPAREN
%token COMMA SEMI SEMISEMI EQUAL
%token <string> LNAME
%token UNDERSCORE
%token <int> INT
%token <string> STRING
%token <bool> BOOL
%token PLUS MINUS STAR
%token <string> PARAM
%token ARROW LARROW RARROW TILDEARROW PUREARROW
%token COLON
%token MATCH WITH EFFECT END
%token LET IN
%token FUN BAR
%token IF THEN ELSE
%token EOF

%nonassoc IN ARROW
%right SEMI
%left  PLUS MINUS
%left  STAR

%start <Syntax.plain_pattern> plain_simple_pattern
%start <Syntax.toplevel list> file
%start <Syntax.toplevel> commandline

%%

(* Toplevel syntax *)

file:
  | lst = file_topdef
    { lst }
  | t = topterm EOF
     { [t] }
  | t = topterm SEMISEMI lst = file
     { t :: lst }

file_topdef:
  | EOF
     { [] }
  | def = topdef SEMISEMI lst = file
     { def :: lst }
  | def = topdef lst = file_topdef
     { def :: lst }

commandline:
  | def = topdef SEMISEMI
    { def }
  | t = topterm SEMISEMI
    { t }

topdef:
  | LET def = let_def
    { Let (fst def, snd def) }

topterm:
  | t = term
    { Term t }

(* Main syntax tree *)

term: mark_position(plain_term)
  { $1 }
plain_term:
  | MATCH t = term WITH cases = cases END
    { Match (t, cases) }
  | FUN t = lambdas1(ARROW)
    { t.plain }
  | LET def = let_def IN t = term
    { Let (fst def, snd def, t) }
  | t1 = term SEMI t2 = term
    { let pat =
        { plain = Nonbinding;
          location = t1.location; }
      in
        Let (pat, t1, t2) }
  | IF t_cond = comma_term THEN t_true = term ELSE t_false = term END
    { Conditional (t_cond, t_true, t_false) }
  | t = plain_comma_term
    { t }

comma_term: mark_position(plain_comma_term) { $1 }
plain_comma_term:
  | t = binop_term COMMA ts = separated_list(COMMA, binop_term)
    { Tuple (t :: ts) }
  | t = plain_binop_term
    { t }

binop_term: mark_position(plain_binop_term) { $1 }
plain_binop_term:
  | t1 = binop_term op = binop t2 = binop_term
    {
      let op_loc = Location.make $startpos(op) $endpos(op) in
      let var = { plain = Var op; location = op_loc } in
      let partial_pos = Location.make $startpos(t1) $endpos(op) in
      let partial = {plain = Apply (var, t1); location = partial_pos} in
      Apply (partial, t2)
    }
  | t = plain_app_term
    { t }

plain_app_term:
  | t = simple_term ts = simple_term+
    {
      let apply ({plain; location = loc1} as t1) ({location = loc2} as t2) =
        { plain = Apply(t1, t2);
          location = Location.merge loc1 loc2}
      in
      let ap = List.fold_left apply t ts in
        ap.plain
    }
  | t = plain_simple_term
    { t }

simple_term: mark_position(plain_simple_term) { $1 }
plain_simple_term:
  | x = ident
    { Var x }
  | cst = const_term
    { Const cst }
  | LPAREN RPAREN
    { Tuple [] }
  | LPAREN t = term COLON ty = ty RPAREN
    { (Constraint(t, ty) : plain_term) }
  | LPAREN t = plain_term RPAREN
    { t }

(* Auxilliary definitions *)

const_term:
  | n = INT
    { Const.of_integer n }
  | str = STRING
    { Const.of_string str }
  | b = BOOL
    { Const.of_boolean b }

cases:
  | BAR? cs = separated_nonempty_list(BAR, case)
    { collect_cases cs }
case: mark_position(plain_case) { $1 }
plain_case:
  | EFFECT op = effect p = simple_pattern k = simple_pattern ARROW t2 = term
    { EffectCase (op, ((p : Syntax.pattern), k, t2)) }
  | p = pattern ARROW t = term
    { ReturnCase((p : Syntax.pattern), t) }

lambdas0(SEP):
  | SEP t = term
    { t }
  | p = simple_pattern t = lambdas0(SEP)
    { { plain = Lambda ((p : Syntax.pattern), t);
        location = Location.make $startpos $endpos } }

lambdas1(SEP):
  | p = simple_pattern t = lambdas0(SEP)
    { { plain = Lambda (p, t);
        location = Location.make $startpos $endpos } }

let_def:
  | p = pattern EQUAL t = term
    { ((p : Syntax.pattern), t) }
  | x = mark_position(ident) t = lambdas1(EQUAL)
    { ({plain = Var x.plain; location = x.location}, t) }

pattern: mark_position(plain_pattern) { $1 }
plain_pattern:
  | ps = separated_nonempty_list(COMMA, simple_pattern)
    { match ps with [(p : Syntax.pattern)] -> p.plain | ps -> Tuple ps }

simple_pattern: mark_position(plain_simple_pattern) { $1 }

plain_simple_pattern:
  | x = ident
    { Var x }
  | UNDERSCORE
    { Nonbinding }
  | cst = const_term
    { Const cst }
  | LPAREN RPAREN
    { Tuple [] }
  | LPAREN p = pattern COLON ty = ty RPAREN
    { (Constraint((p : Syntax.pattern), ty) : Syntax.plain_pattern) }
  | LPAREN p = pattern RPAREN
    { p.plain }

effect:
  | s = LNAME
    { effect s }

tyname:
  | s = LNAME
    { tyname s }

ident:
  | s = LNAME
    { variable s }

rowparam:
  | s = PARAM
    { rowparam s }

typaram:
  | s = PARAM
    { typaram s }

%inline binop:
  | PLUS
    { variable "+" }
  | MINUS
    { variable "-" }
  | STAR
    { variable "*" }

mark_position(PLAIN):
  plain = PLAIN
  { {plain; location = Location.make $startpos $endpos} }

ty: mark_position(plain_ty) {$1}
plain_ty:
  | t1 = prod_ty LARROW d = dirt RARROW t2 = ty
    { TyArrow (t1, t2, d) }
  | t1 = prod_ty ARROW t2 = ty
    { TyArrow (t1, t2, io_dirt) }
  | t1 = prod_ty PUREARROW t2 = ty
    { TyArrow (t1, t2, pure_dirt) }
  | t1 = prod_ty TILDEARROW t2 = ty
    { TyArrow (t1, t2, tilde_dirt) }
  | t = plain_prod_ty
    { t }

prod_ty: mark_position(plain_prod_ty) { $1 }
plain_prod_ty:
  | ts = separated_nonempty_list(STAR, simple_ty)
    {
      match ts with
      | [] -> assert false
      | [t] -> t.plain
      | _ -> TyTuple ts
     }

simple_ty: mark_position(plain_simple_ty) { $1 }
plain_simple_ty:
  | t = tyname
    { TyBasic t }
  | t = typaram
    { TyParam t }
  | LPAREN t = ty RPAREN
    { t.plain }

dirt:
  | ops = separated_nonempty_list(BAR, effect)
    { {operations = ops; row = None;} }
  | ops = separated_nonempty_list(BAR, effect) row = rowparam
    { {operations = ops; row = Some row;} }

%%
