(** Abstract syntax of eff terms, types, and toplevel commands. *)

type variable = string (** variable identifiers *)
type operation = string (** operation symbols *)
type label = string (** variant labels *)

type tyname = string (** type names *)
type typaram = string (** type parameters *)
type dirtparam = string (** dirt parameters *)

(** Terms *)
type term = plain_term * Location.t
and plain_term =
  | Var of variable
  (** variables *)
  | Const of const
  (** integers, strings, booleans, and floats *)
  | Tuple of term list
  (** [(t1, t2, ..., tn)] *)
  | Variant of label * term option
  (** [Label] or [Label t] *)
  | Lambda of abstraction
  (** [fun p1 p2 ... pn -> t] *)
  | Operation of operation
  (** [t#op], where [op] is an operation symbol. *)
  | Let of variable Pattern.t * term * term
  (** [let p1 = t1 and ... and pn = tn in t] *)
  | Conditional of term * term * term
  (** [if t then t1 else t2] *)
  | Apply of term * term
  (** [t1 t2] *)
  | Match of term * cases
  (** [with t1 handle t2] *)

and cases = {
  operations : (operation, abstraction2) assoc;
  (** [t1#op1 p1 k1 -> t1' | ... | tn#opn pn kn -> tn'] *)
  value : abstraction list;
  (** [val p -> t] *)
}

and abstraction = variable Pattern.t * term

and abstraction2 = variable Pattern.t * variable Pattern.t * term

type dirt = {
  ops : operation list;
  row : dirtparam option;
}


type ty = plain_ty * Location.t
and plain_ty =
  | TyBasic of tyname
  | TyParam of typaram
  (** ['a] *)
  | TyArrow of ty * ty * dirt
  (** [ty1 -> ty2] *)
  | TyTuple of ty list
  (** [ty1 * ty2 * ... * tyn] *)
