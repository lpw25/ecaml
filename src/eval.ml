open Syntax

exception PatternMatch

let rec extend_with_pattern p v env =
  match p.plain, v with
  | Nonbinding, _ -> env
  | Var x, v -> Map.update x v env
  | Const c, Value.Const c' when c = c' -> env
  | Tuple ps, Value.Tuple vs -> List.fold_right2 extend_with_pattern ps vs env
  | _, _ -> raise PatternMatch

let extend env p v =
  try
    extend_with_pattern p v env
  with
    PatternMatch -> Error.runtime ~loc:p.location "Pattern match failure."

let rec sequence r k =
  match r with
  | Value.Value v -> k v
  | Value.Perform (eff, param, k') ->
    Value.Perform (eff, param, fun x -> sequence (k' x) k)

let rec eval env t =
  match t.plain with
  | Var x ->
    begin match Map.lookup x env with
      | None -> Error.runtime ~loc:t.location "Unknown identifier."
      | Some v -> Value.Value v
    end
  | Const c -> Value.Value (Value.Const c)
  | Tuple ts ->
    let fold t k =
      fun vs ->
        sequence (eval env t) (fun v -> k (v :: vs))
    in
    List.fold_right fold ts (fun vs -> Value.Value (Value.Tuple (List.rev vs))) []
  | Lambda a -> Value.Value (Value.Closure (eval_abstraction env a))
  | Apply (t1, t2) ->
    sequence (eval env t1) (fun v1 ->
        match v1 with
        | Value.Closure f -> sequence (eval env t2) f
        | _ -> Error.runtime ~loc:t1.location "A function expected in application."
      )
  | Conditional (t_if, t_then, t_else) ->
    sequence (eval env t_if) (fun v ->
        match v with
        | Value.Const (Const.Boolean b) -> eval env (if b then t_then else t_else)
        | _ -> Error.runtime ~loc:t_if.location "A boolean expected in conditional."
      )
  | Perform (eff, param) ->
    sequence (eval env param) (fun v ->
        Value.Perform (eff, v, fun x -> Value.Value x))
  | Match (t, {effects; values}) ->
    let rec h = function
      | Value.Value v -> eval_cases ~loc:t.location env values v
      | Value.Perform (eff, v, k) ->
        let k' u = h (k u) in
        begin match Map.lookup eff effects with
          | Some a2 -> eval_abstraction2 env a2 v (Value.Closure k')
          | None -> Value.Perform (eff, v, k')
        end
    in
    h (eval env t)
  | Let (p, t1, t2) ->
    sequence (eval env t1) (eval_abstraction env (p, t2))
  | Constraint (t, _) -> eval env t

and eval_abstraction env (p, t) =
  fun v -> eval (extend env p v) t

and eval_abstraction2 env (p1, p2, t) =
  fun v1 -> fun v2 -> eval (extend (extend env p1 v1) p2 v2) t

and eval_cases ~loc env cases v =
  match cases with
  | [] -> Error.runtime ~loc "No branches succeeded in a pattern match."
  | (p, t) :: cases ->
    try eval (extend_with_pattern p v env) t
    with PatternMatch -> eval_cases ~loc env cases v

