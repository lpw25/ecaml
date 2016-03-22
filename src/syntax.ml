(** Abstract syntax of ecaml terms and types. *)

(** Symbols *)
type variable = string   (** variable identifiers *)
type effect = string     (** effect symbols *)
type label = string      (** variant labels *)
type tyname = string     (** type names *)
type typaram = string    (** type parameters *)
type rowparam = string   (** row parameters *)

let variable s = s
let effect s = s
let label s = s
let tyname s = s
let typaram s = s
let rowparam s = s

(** Types *)
type dirt = {
  operations : effect list;
  row : rowparam option;
}

type 'a loc =
  { plain : 'a; location : Location.t }

type ty = plain_ty loc

and plain_ty =
  | TyParam of typaram
  (** ['a] *)
  | TyBasic of tyname
  (** [bool], [int], ... *)
  | TyTuple of ty list
  (** [ty1 * ty2 * ... * tyn] *)
  | TyArrow of ty * ty * dirt
  (** [ty1 -[d]-> ty2] *)


(** Patterns *)
type pattern = plain_pattern loc

and plain_pattern =
  | Nonbinding
  | Var of variable
  | Const of Const.t
  | Tuple of pattern list
  | Constraint of pattern * ty

(** Terms *)
type term = plain_term loc

and plain_term =
  | Var of variable
  | Const of Const.t
  | Tuple of term list
  | Lambda of abstraction
  | Apply of term * term
  | Conditional of term * term * term
  | Perform of effect * term
  | Match of term * cases
  | Let of pattern * term * term
  | Constraint of term * ty

and cases = {
  effects : (effect, abstraction2) Map.t;
  (** [effect E1 p1 k1 -> t1 | ... | effect En pn kn -> tn] *)
  values : abstraction list;
  (** [p1 -> t1 | ... | pn -> tn] *)
}

and abstraction = pattern * term

and abstraction2 = pattern * pattern * term

(** Toplevel commands *)
type toplevel =
  | Term of term
  | Let of pattern * term
