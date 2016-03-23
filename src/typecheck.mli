(* Typing environments *)
type env
val empty : env

(* Unification variables representing types and dirt *)
type tyvar
type dirtvar
val print_type_and_effect : Format.formatter -> (tyvar * dirtvar) -> unit


(* Type inference *)
val extend_poly_env : loc:Location.t -> env -> tyvar -> Syntax.pattern -> env
val infer : env -> Syntax.term -> (tyvar * dirtvar)

