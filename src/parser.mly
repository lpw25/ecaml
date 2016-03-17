%{
  open Syntax

  type handler_case =
    | EffectCase of operation * abstraction2
    | ReturnCase of abstraction

  let collect_handler_cases (lst : (handler_case * Location.t) list) =
    let (ops, ret) =
      List.fold_left
        (fun (ops, ret, fin) -> function
          | (OperationCase (op, a2), _) ->  ((op, a2) :: ops, ret)
          | (ReturnCase a, loc) -> (ops, a :: ret))
        ([], None)
        lst
    in
    { operations = List.rev ops;
      value = ret; }

  let io_dirt = { operations = ["io"]; row = None }
  let tilde_dirt = { operations = [""]; row = Some "~" }
  let pure_dirt = { operations = []; row = None }

%}

%token LPAREN RPAREN
%token COMMA SEMI SEMISEMI EQUAL
%token <Common.variable> LNAME
%token UNDERSCORE
%token <Big_int.big_int> INT
%token <string> STRING
%token <bool> BOOL
%token PLUS MINUS STAR
%token <Common.typaram> PARAM
%token ARROW ARRSTART ARREND TILDEARROW PUREARROW
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

%start <Syntax.term> commandline

%%

(* Toplevel syntax *)

(* If you're going to "optimize" this, please make sure we don't require;; at the
   end of the file. *)
commandline:
  | t = term SEMISEMI
    { t }
  | t = term EOF
    { t }

(* Main syntax tree *)

term: mark_position(plain_term) { $1 }
plain_term:
  | MATCH t = term WITH cases = cases END
    { Match (t, cases) }
  | FUN t = lambdas1(ARROW)
    { fst t }
  | LET def = let_def IN t = term
    { Let (defs, t) }
  | t1 = term SEMI t2 = term
    { Let ([(Pattern.Nonbinding, snd t1), t1], t2) }
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
      let partial = Apply ((Var op, op_loc), t1) in
      let partial_pos = Location.make $startpos(t1) $endpos(op) in
      Apply ((partial, partial_pos), t2)
    }
  | t = plain_app_term
    { t }

plain_app_term:
  | t = simple_term ts = simple_term+
    {
      let apply ((_, loc1) as t1) ((_, loc2) as t2) =
        (Apply(t1, t2), Location.join loc1 loc2)
      in
        fst (List.fold_left apply t ts)
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
  | LPAREN t = plain_term RPAREN
    { t }
  | LPAREN t = term COLON ty RPAREN
    { Constraint(p, t) }


(* Auxilliary definitions *)

const_term:
  | n = INT
    { Common.Integer n }
  | str = STRING
    { Common.String str }
  | b = BOOL
    { Common.Boolean b }

cases:
  | BAR? cs = separated_nonempty_list(BAR, case)
    { cs }
case: mark_position(plain_case) { $1 }
plain_case:
  | EFFECT op = ident p = simple_pattern k = simple_pattern ARROW t2 = term
    { OperationCase ((t1, op), (p, k, t2)) }
  | p = pattern ARROW t = term
    { ReturnCase(p, t) }

lambdas0(SEP):
  | SEP t = term
    { t }
  | p = simple_pattern t = lambdas0(SEP)
    { (Lambda (p, t), Location.make $startpos $endpos) }

lambdas1(SEP):
  | p = simple_pattern t = lambdas0(SEP)
    { (Lambda (p, t), Location.make $startpos $endpos) }

let_def:
  | p = pattern EQUAL t = term
    { (p, t) }
  | x = mark_position(ident) t = lambdas1(EQUAL)
    { ((Pattern.Var (fst x), (snd x)), t) }

pattern: mark_position(plain_pattern) { $1 }
plain_pattern:
  | ps = separated_nonempty_list(COMMA, simple_pattern)
    { match ps with [(p, _)] -> p | ps -> Pattern.Tuple ps }

simple_pattern: mark_position(plain_simple_pattern) { $1 }
plain_simple_pattern:
  | x = ident
    { Pattern.Var x }
  | UNDERSCORE
    { Pattern.Nonbinding }
  | cst = const_term
    { Pattern.Const cst }
  | LPAREN RPAREN
    { Pattern.Tuple [] }
  | LPAREN p = pattern RPAREN
    { p }
  | LPAREN p = pattern COLON ty RPAREN
    { Pattern.Constraint(p, t) }

lname:
  | x = LNAME
    { x }

tyname:
  | t = lname
    { t }

ident:
  | x = lname
    { x }

%inline binop:
  | PLUS
    { "+" }
  | MINUS
    { "-" }
  | STAR
    { "*" }

mark_position(X):
  x = X
  { x, Location.make $startpos $endpos}

ty: mark_position(plain_ty) { $1 }
plain_ty:
  | t1 = prod_ty ARRSTART d = dirt ARREND t2 = ty
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
  | ts = separated_nonempty_list(STAR, plain_simple_ty)
    {
      match ts with
      | [] -> assert false
      | [t] -> fst t
      | _ -> TyTuple ts
     }

plain_simple_ty:
  | t = tyname
    { TyBasic (t, [], None, None) }
  | t = PARAM
    { TyParam t }
  | LPAREN t = ty RPAREN
    { fst t }

dirt:
  | ops = separated_nonempty_list(BAR, ident)
    { {operations = ops; row = None;} }
  | ops = separated_nonempty_list(BAR, ident) row = PARAM
    { {operations = ops; row = Some row;} }

%%
