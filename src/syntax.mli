(** Abstract syntax of ecaml terms and types. *)

(** Symbols *)
type variable   (** variable identifiers *)
type effect     (** effect symbols *)
type label      (** variant labels *)
type tyname     (** type names *)
type typaram    (** type parameters *)
type rowparam   (** row parameters *)

(** Types *)
type dirt = {
  operations : effect list;
  row : rowparam option;
}

type ty = {
  ty: plain_ty;
  location: Location.t;
}
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
type pattern = {
  pattern: plain_pattern;
  location: Location.t;
}
and plain_pattern =
  | Nonbinding
  | Var of variable
  | Const of Const.t
  | Tuple of pattern list

(** Terms *)
type term = {
  term: plain_term;
  location: Location.t;
}
and plain_term =
  | Var of variable
  (** variables *)
  | Const of Const.t
  (** integers, booleans, ... *)
  | Tuple of term list
  (** [(t1, t2, ..., tn)] *)
  | Lambda of abstraction
  (** [fun p -> t] *)
  | Apply of term * term
  (** [t1 t2] *)
  | Perform of effect
  (** [perform E] *)
  | Match of term * cases
  (** [match t1 with t2] *)

and cases = {
  effects : (effect, abstraction2) Map.t;
  (** [effect E1 p1 k1 -> t1 | ... | effect En pn kn -> tn] *)
  values : abstraction list;
  (** [p1 -> t1 | ... | pn -> tn] *)
}

and abstraction = pattern * term

and abstraction2 = pattern * pattern * term
