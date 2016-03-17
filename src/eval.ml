open Syntax

exception PatternMatch

let rec extend_with_pattern p v env =
  match p.pattern, v with
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

let rec extend_with_cases ~loc env cases v =
  match cases with
  | [] -> Error.runtime ~loc "No branches succeeded in a pattern match."
  | (p, t) :: cases ->
    try extend_with_pattern p v env
    with PatternMatch -> extend_with_cases ~loc env cases v

let rec sequence r k =
  match r with
  | Value.Value v -> k v
  | Value.Perform (eff, param, k') ->
    Value.Perform (eff, param, fun x -> sequence (k' x) k)

let rec eval env t =
  match t.term with
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
    List.fold_right fold ts (fun vs -> Value.Value (Value.Tuple vs)) []
  | Lambda a -> Value.Value (Value.Closure (eval_abstraction env a))
  | Apply (t1, t2) ->
    sequence (eval env t1) (fun v1 ->
        match v1 with
        | Value.Closure f -> sequence (eval env t2) f
        | _ -> Error.runtime ~loc:t.location "A closure expected."
      )
  | Perform (eff, param) ->
    sequence (eval env param) (fun v ->
        Value.Perform (eff, v, fun x -> Value.Value x))
  | Match (t, {effects; values}) ->
    let rec h = function
      | Value.Value v -> eval (extend_with_cases ~loc:t.location env values v) t
      | Value.Perform (eff, v, k) ->
        let k' u = h (k u) in
        begin match Map.lookup eff effects with
          | Some a2 -> eval_abstraction2 env a2 v (Value.Closure k')
          | None -> Value.Perform (eff, v, k')
        end
    in
    h (eval env t)

and eval_abstraction env (p, t) =
  fun v -> eval (extend env p v) t

and eval_abstraction2 env (p1, p2, t) =
  fun v1 -> fun v2 -> eval (extend (extend env p1 v1) p2 v2) t
