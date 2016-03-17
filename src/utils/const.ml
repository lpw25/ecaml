(** Built in constants *)

type t =
  | Integer of int
  | String of string
  | Boolean of bool
  | Float of float

let print c ppf =
  match c with
  | Integer k -> Format.fprintf ppf "%d" k
  | String s -> Format.fprintf ppf "%S" s
  | Boolean b -> Format.fprintf ppf "%B" b
  | Float f -> Format.fprintf ppf "%F" f

let of_integer n = Integer n
let of_string s = String s
let of_boolean b = Boolean b
let of_float f = Float f
