open Syntax

(*
 * Unification variables
 *)

type 'a univar_status =
| Bound of 'a univar
| Def of 'a

and 'a univar = {
  mutable status : 'a univar_status;
}

let var s = { status = Def s }

(* union-find data structure *)
let rec defn_of_var v = match v.status with
  | Bound v' ->
     let (d, v') as ret = defn_of_var v' in
     v.status <- Bound v';
     ret
  | Def d ->
     (d, v)


(*
 * Unification of types
 *)

type type_defn =
| Fresh
| UBasic of tyname
| UTuple of tyvar list
| UArrow of tyvar * tyvar * dirt

and tyvar = type_defn univar

let of_dirt d = d

let rec of_ty names t = match t.plain with
  | TyParam p -> 
     (match List.assoc p !names with
     | v -> v
     | exception Not_found ->
        let v = var Fresh in
        names := (p, v) :: !names;
        v)
  | TyBasic n -> var (UBasic n)
  | TyTuple ts -> var (UTuple (List.map (of_ty names) ts))
  | TyArrow (t, t', d) ->
     var (UArrow (of_ty names t, of_ty names t', of_dirt d))

let rec print_tyvar names must_delim ppf v =
  let (d, v) = defn_of_var v in
  match d with
  | Fresh -> 
     let id = match List.assq v !names with
       | n -> n
       | exception Not_found ->
          let n = List.length !names in
          names := (v, n) :: !names;
          n in
     if id < 26 then
       Format.fprintf ppf "'%c" (Char.chr (Char.code 'a' + id))
     else
       Format.fprintf ppf "'v_%d" (id - 26)
  | UBasic p ->
     Format.fprintf ppf "%s" (p :> string)
  | UTuple ts ->
     let comma ppf () = Format.fprintf ppf ", " in
     Format.fprintf ppf "(%a)"
       (Format.pp_print_list ~pp_sep:comma (print_tyvar names true)) ts
  | UArrow (t1, t2, d) ->
     let p ppf () =
       Format.fprintf ppf "%a -[?]-> %a"
         (print_tyvar names true) t1
         (print_tyvar names must_delim) t2 in
     if must_delim then
       Format.fprintf ppf "(%a)" p ()
     else
       Format.fprintf ppf "%a" p ()

let print ppf v =
  let names = ref [] in
  print_tyvar names false ppf v

let unify_dirt d d' = ()

let rec occur v a =
  assert (match v.status with Bound _ -> false | Def _ -> true);
  let (d, v') = defn_of_var a in
  if v == v' then true else
  match d with
  | Fresh | UBasic _ -> false
  | UTuple ts -> List.exists (occur v) ts
  | UArrow (t1, t2, d) -> occur v t1 || occur v t2

let bind loc v v' =
  if occur v v' then 
    Error.typing ~loc 
      "Occurs check failed: 'a occurs in %a"
      (print_tyvar (ref [v, 0]) false) v';
  assert (v.status = Def Fresh);
  v.status <- Bound v'

let rec unify loc a a' = 
  let (d, v) = defn_of_var a and (d', v') = defn_of_var a' in
  if v == v' then () else
  match d, d' with
  | Fresh, _ -> bind loc v v'
  | _, Fresh -> bind loc v' v
  | UBasic p, UBasic p' when p = p' -> ()
  | UTuple ts, UTuple ts' -> List.iter2 (unify loc) ts ts'
  | UArrow (t1, t2, d), UArrow (t1', t2', d') -> 
     unify loc t1 t1'; unify loc t2 t2'; unify_dirt d d'
  | _, _ -> 
     let names = ref [] in
     Error.typing ~loc "%a does not unify with %a" 
       (print_tyvar names false) a 
       (print_tyvar names false) a'


(*
 * Environments, instantiation and generalisation
 *)

type binding =
| Mono of tyvar
| Poly of tyvar list * tyvar

type env =
| Empty
| Marker of env
| Bind of Syntax.variable * binding * env

let empty = Empty

let rec lookup loc (v : variable) = function
| Empty -> Error.typing ~loc "no such variable %s" (v :> string)
| Bind (v', b, env) when v = v' -> b
| Marker env | Bind (_, _, env) -> lookup loc v env

let instantiate = function
  | Mono v -> v
  | Poly (ps, v) ->
     let universals = ps |>
         List.map (fun v ->
           let (d, v) = defn_of_var v in
           assert (d = Fresh);
           (v, var Fresh)) in
     let inst_dirt d = d in
     let rec inst v = 
       let (d, v) = defn_of_var v in
       match List.assq v universals with
       | v' -> v'
       | exception Not_found -> match d with
         | Fresh -> v
         | UBasic p -> var (UBasic p)
         | UTuple ts -> var (UTuple (List.map inst ts))
         | UArrow (t1, t2, d) -> var (UArrow (inst t1, inst t2, inst_dirt d)) in
     inst v

(* Generalise all monomorphic bindings after the last Marker *)
let rec generalise env =
  (* awful environment-scanning nonense. Levels would be better. *)
  let rec free_in_type acc v =
    let (d, v) = defn_of_var v in
    match d with
    | Fresh -> v :: acc
    | UBasic _ -> acc
    | UTuple ts -> List.fold_left free_in_type acc ts
    | UArrow (t1, t2, d) -> free_in_type (free_in_type acc t1) t2 in

  let free_in_binding acc = function
    | Mono v -> free_in_type acc v
    | Poly (ps, v) ->
       List.filter (fun p -> not (List.memq p ps)) (free_in_type [] v) @ acc in

  let rec free_in_env acc = function
    | Empty -> acc
    | Marker env -> free_in_env acc env
    | Bind (v, b, env) -> free_in_env (free_in_binding acc b) env in

  let set_diff add remove =
    let rec diff acc remove = function
      | [] -> acc
      | x :: xs ->
         if not (List.memq x acc) && not (List.memq x remove) then
           diff (x :: acc) remove xs
         else
           diff acc remove xs in
    diff [] remove add in

  let rec gen = function
    | Empty -> assert false (* no marker?? *)
    | Marker env -> (free_in_env [] env, env)
    | Bind (v, b, env) ->
       let (free, env') = gen env in
       let b' = match b with
         | Mono v -> Poly (set_diff (free_in_type [] v) free, v)
         | Poly _ as p -> p in
       (free, Bind (v, b', env')) in

  let (free, env') = gen env in env'


(*
 * Type inference
 *)

let type_of_const k =
  var (UBasic (tyname Const.(match k with 
    | Integer _ -> "int"
    | String _ -> "string"
    | Boolean _ -> "boolean")))

let rec extend_env loc (env : env) exp pat = match pat.plain with
  | Nonbinding -> env
  | Var v -> Bind (v, Mono exp, env)
  | Const k -> unify loc exp (type_of_const k); env
  | Tuple pats ->
     let comps = List.map (fun p -> var Fresh) pats in
     unify loc exp (var (UTuple comps));
     List.fold_left2 (extend_env loc) env comps pats
  | Constraint (pat, ty) ->
     unify loc exp (of_ty (ref []) ty);
     extend_env loc env exp pat

let pure : dirt = { operations = []; row = None }

let rec infer env t = match t.plain with
  | Var v -> instantiate (lookup t.location v env)
  | Const k -> type_of_const k
  | Tuple ts -> var (UTuple (List.map (infer env) ts))
  | Lambda (pat, t) ->
     let v = var Fresh in
     var (UArrow (v, infer (extend_env t.location env v pat) t, pure))
  | Apply (fn, arg) ->
     let tfn = infer env fn and targ = infer env arg in
     let a = var Fresh and b = var Fresh in
     unify t.location tfn (var (UArrow (a, b, pure)));
     unify t.location targ a;
     b
  | Conditional (cond, texp, fexp) ->
     unify t.location (infer env cond) (var (UBasic (tyname "boolean")));
     let ta = infer env texp and tb = infer env fexp in
     unify t.location ta tb; ta
  | Perform (e, arg) ->
     (* FIXME: effects are untyped *)
     let _ = infer env arg in
     var Fresh
  | Match (scrutinee, {effects ; values}) ->
     (* FIXME: effects are untyped *)
     let ts = infer env scrutinee in
     let res = var Fresh in
     values |> List.iter (fun (pat, t) ->
       unify t.location res (infer (extend_env t.location env ts pat) t));
     res
  | Let (pat, t, body) ->
     infer (generalise (extend_env t.location (Marker env) (infer env t) pat)) body
  | Constraint (term, ty) ->
     let ty' = infer env term in
     unify t.location ty' (of_ty (ref []) ty);
     ty'
