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
 * Unification of types and dirt
 *)

type dirt_defn =
| Rowvar
| Closed
| UEff of effect * dirtvar

and dirtvar = dirt_defn univar

type type_defn =
| Fresh
| UBasic of tyname
| UTuple of tyvar list
| UArrow of tyvar * tyvar * dirtvar

and tyvar = type_defn univar

let by_name names fresh p =
  match List.assoc p !names with
  | v -> v
  | exception Not_found ->
     let v = var fresh in
     names := (p, v) :: !names;
     v

let rec of_dirt dnames = function
  | { operations = []; row = None } ->
     var Closed
  | { operations = []; row = Some r } ->
     by_name dnames Rowvar r
  | { operations = eff :: effs; row } ->
     var (UEff (eff, of_dirt dnames { operations = effs; row }))

let rec of_ty names dnames t = match t.plain with
  | TyParam p -> by_name names Fresh p
  | TyBasic n -> var (UBasic n)
  | TyTuple ts -> var (UTuple (List.map (of_ty names dnames) ts))
  | TyArrow (t, t', d) ->
     var (UArrow (of_ty names dnames t, of_ty names dnames t', of_dirt dnames d))

let to_name v names =
   match List.assq v !names with
   | n -> n
   | exception Not_found ->
      let n = List.length !names in
      names := (v, n) :: !names;
      n

let rec print_dirt dnames sep ppf v =
  let (d, v) = defn_of_var v in
  match d with
  | Rowvar ->
    (match to_name v dnames with
    | 0 ->
       Format.fprintf ppf "%s~" sep
    | 1 ->
       Format.fprintf ppf "%s'p" sep
    | n ->
       Format.fprintf ppf "%s'p%d" sep n)
  | Closed -> ()
  | UEff (eff, tail) ->
     Format.fprintf ppf "%s%s%a" sep
       (eff :> string)
       (print_dirt dnames " | ") tail

let rec print_tyvar names dnames must_delim ppf v =
  let (d, v) = defn_of_var v in
  match d with
  | Fresh -> 
     let id = to_name v names in
     if id < 26 then
       Format.fprintf ppf "'%c" (Char.chr (Char.code 'a' + id))
     else
       Format.fprintf ppf "'v_%d" (id - 26)
  | UBasic p ->
     Format.fprintf ppf "%s" (p :> string)
  | UTuple ts ->
     let comma ppf () = Format.fprintf ppf ", " in
     Format.fprintf ppf "(%a)"
       (Format.pp_print_list ~pp_sep:comma (print_tyvar names dnames true)) ts
  | UArrow (t1, t2, d) ->
     let print_dirty_arrow ppf v =
       let (d, v) = defn_of_var v in
       match d with
       | Closed -> 
          Format.fprintf ppf "=>"
       | Rowvar when to_name v dnames = 0 ->
          Format.fprintf ppf "~>"
       | _ ->
          Format.fprintf ppf "-[%a]>"
            (print_dirt dnames "") v in
     let p ppf () =
       Format.fprintf ppf "%a %a %a"
         (print_tyvar names dnames true) t1
         print_dirty_arrow d
         (print_tyvar names dnames must_delim) t2 in
     if must_delim then
       Format.fprintf ppf "(%a)" p ()
     else
       Format.fprintf ppf "%a" p ()

let print_type_and_effect ppf (ty, dirt) =
  let names = ref [] and dnames = ref [] in
  Format.fprintf ppf "%a ! [%a]"
    (print_tyvar names dnames false) ty
    (print_dirt dnames "") dirt

let rec occur f v a =
  assert (match v.status with Bound _ -> false | Def _ -> true);
  let (d, v') = defn_of_var a in
  if v == v' then true else f (occur f v) d

let occur_dirt = occur (fun f -> function
  | Rowvar | Closed -> false
  | UEff (_, d) -> f d)

let rec list_of_dirt acc dirt =
  let (d, dirt) = defn_of_var dirt in
  match d with
  | Rowvar -> (List.rev acc, Some dirt)
  | Closed -> (List.rev acc, None)
  | UEff (e, dirt) -> list_of_dirt (e :: acc) dirt

let rec dirt_of_list effs row =
  match effs, row with
  | (e :: es), row -> var (UEff (e, dirt_of_list es row))
  | [], Some rowvar -> rowvar
  | [], None -> var Closed


let rec unify_dirt loc dirt dirt' =
  let (effs, row) = list_of_dirt [] dirt in
  let (effs', row') = list_of_dirt [] dirt' in

  let rec remove_one y = function
    | [] -> failwith "removing an absent element"
    | x :: xs -> if x = y then xs else x :: remove_one y xs in
  let rec remove_common effs = function
    | [] -> effs, []
    | eff' :: effs' ->
       if List.mem eff' effs then
         remove_common (remove_one eff' effs) effs'
       else
         let (diff, diff') = remove_common effs effs' in
         (diff, eff' :: diff') in
  let (effs, effs') = remove_common effs effs' in

  let bind_dirt loc d d' =
    assert (d.status = Def Rowvar);
    assert (d != d');
    assert (not (occur_dirt d d'));
    d.status <- Bound d' in

  match effs, row, effs', row' with
  | [], None, [], None ->
     (* OK: unifying two equal closed effects *)
     ()
  | _, None, (_ :: _), _
  | (_ :: _), _, _, None ->
     (* BAD: trying to insert an effect into a closed effect *)
     Error.typing ~loc "Cannot add to closed effects"
  | [], Some row, [], Some row' when row == row' ->
     (* OK: same row, equal effects: no change *)
     ()
  | _, Some row, _, Some row' when row == row' ->
     (* BAD: same row but different effects: occurs check fails *)
     Error.typing ~loc "Occurs check failed on effects"
  | effs, None, [], Some row
  | [], Some row, effs, None ->
     (* OK: unifying a row variable with a closed effect *)
     bind_dirt loc row (dirt_of_list effs None)
  | effs, Some row, effs', Some row' ->
     (* OK: unifying two different row variables *)
     let res = Some (var Rowvar) in
     bind_dirt loc row (dirt_of_list effs' res);
     bind_dirt loc row' (dirt_of_list effs res)

let unify_dirts loc dirts =
  let dirt = var Rowvar in
  dirts |> List.iter (unify_dirt loc dirt);
  dirt

let occur_ty = occur (fun f -> function
  | Fresh | UBasic _ -> false
  | UTuple ts -> List.exists f ts
  | UArrow (t1, t2, d) -> f t1 || f t2)

let bind_ty loc v v' =
  assert (v.status = Def Fresh && v != v');
  if occur_ty v v' then 
    Error.typing ~loc 
      "Occurs check failed: 'a occurs in %a"
      (print_tyvar (ref [v, 0]) (ref []) false) v';
  v.status <- Bound v'

let rec unify loc a a' = 
  let (d, v) = defn_of_var a and (d', v') = defn_of_var a' in
  if v == v' then () else
  match d, d' with
  | Fresh, _ -> bind_ty loc v v'
  | _, Fresh -> bind_ty loc v' v
  | UBasic p, UBasic p' when p = p' -> ()
  | UTuple ts, UTuple ts' -> List.iter2 (unify loc) ts ts'
  | UArrow (t1, t2, d), UArrow (t1', t2', d') -> 
     unify loc t1 t1'; unify loc t2 t2'; unify_dirt loc d d'
  | _, _ -> 
     let names = ref [] and dnames = ref [] in
     Error.typing ~loc "%a does not unify with %a" 
       (print_tyvar names dnames false) a 
       (print_tyvar names dnames false) a'


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
     unify loc exp (of_ty (ref []) (ref []) ty);
     extend_env loc env exp pat

(* For now, effects are presumed to be int -> int *)
let eff_arg eff = var (UBasic (tyname "int"))
let eff_res eff = var (UBasic (tyname "int"))

let extend_poly_env ~loc env ty pat =
  generalise (extend_env loc (Marker env) ty pat)

let rec infer env t = match t.plain with
  | Var v -> instantiate (lookup t.location v env), var Rowvar
  | Const k -> type_of_const k, var Rowvar
  | Tuple ts -> 
     let dirt = var Rowvar in
     var (UTuple (List.map (fun t -> 
       let (ty, dirt') = infer env t in
       unify_dirt t.location dirt dirt';
       ty) ts)), dirt
  | Lambda (pat, t) ->
     let v = var Fresh in
     let (ty, dirt) = infer (extend_env t.location env v pat) t in
     var (UArrow (v, ty, dirt)), var Rowvar
  | Apply (fn, arg) ->
     let (tyfn, dirtfn) = infer env fn and (tyarg, dirtarg) = infer env arg in
     let a = var Fresh and b = var Fresh and eff = var Rowvar in
     unify t.location tyfn (var (UArrow (a, b, eff)));
     unify t.location tyarg a;
     b, unify_dirts t.location [eff; dirtfn; dirtarg]
  | Conditional (cond, texp, fexp) ->
     let (tycond, dirtcond) = infer env cond in
     unify t.location tycond (var (UBasic (tyname "boolean")));
     let (ta, dirta) = infer env texp and (tb, dirtb) = infer env fexp in
     unify t.location ta tb;
     ta, unify_dirts t.location [dirtcond; dirta; dirtb]
  | Perform (eff, arg) ->
     let (ty, dirt) = infer env arg in
     unify t.location ty (eff_arg eff);
     eff_res eff, unify_dirts t.location [dirt; var (UEff (eff, var Rowvar))]
  | Match (scrutinee, {effects ; values}) ->
     let efflist = Map.keys effects in
     let rowvar = var Rowvar in
     let (ty_scrutinee, dirt_scrutinee) = infer env scrutinee in
     unify_dirt t.location dirt_scrutinee (dirt_of_list efflist (Some rowvar));
     let res = var Fresh in
     values |> List.iter (fun (pat, term) ->
       let (res', dirt') = infer (extend_env t.location env ty_scrutinee pat) term in
       unify t.location res res'; unify_dirt t.location rowvar dirt');
     effects |> Map.iter (fun eff (pat_val, pat_cont, term) ->
       let ty_cont = var (UArrow (eff_res eff, ty_scrutinee, rowvar)) in
       let env = extend_env t.location env (eff_arg eff) pat_val in
       let env = extend_env t.location env ty_cont pat_cont in
       let (res', dirt') = infer env term in
       unify t.location res res'; unify_dirt t.location rowvar dirt');
     res, rowvar
  | Let (pat, t, body) ->
     let (ty, dirt) = infer env t in
     let (ty', dirt') = infer (extend_poly_env ~loc:t.location env ty pat) body in
     ty', unify_dirts t.location [dirt; dirt']
  | Constraint (term, ty') ->
     let (ty, dirt) = infer env term in
     unify t.location ty (of_ty (ref []) (ref []) ty');
     ty, dirt

