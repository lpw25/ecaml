(** Abstract syntax of ecaml terms and types. *)

(** Symbols *)
type variable = private string   (** variable identifiers *)
type effect = private string     (** effect symbols *)
type label = private string      (** variant labels *)
type tyname = private string     (** type names *)
type typaram = private string    (** type parameters *)
type rowparam = private string   (** row parameters *)

val variable : string -> variable
val effect : string -> effect
val label : string -> label
val tyname : string -> tyname
val typaram : string -> typaram
val rowparam : string -> rowparam

type 'a loc =
  { plain : 'a; location : Location.t }

(** Types *)
type dirt = {
  operations : effect list;
  row : rowparam option;
}

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
