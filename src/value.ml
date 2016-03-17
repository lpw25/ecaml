type value =
  | Const of Const.t
  | Tuple of value list
  | Closure of closure

and result =
  | Value of value
  | Perform of Syntax.effect * value * closure

and closure = value -> result

let rec print v ppf =
  match v with
  | Const c -> Const.print c ppf
  | Tuple vs -> Print.print ppf "(@[<hov>%t@])" (Print.sequence "," print vs)
  | Closure _ -> Print.print ppf "<fun>"
