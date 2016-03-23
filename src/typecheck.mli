(* Typing environments *)
type env
val empty : env

(* Unification variables representing types *)
type tyvar
val print : Format.formatter -> tyvar -> unit

(* Type inference *)
val infer : env -> Syntax.term -> tyvar

val extend_poly_env : loc:Location.t -> env -> tyvar -> Syntax.pattern -> env