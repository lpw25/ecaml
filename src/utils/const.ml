(** Built in constants *)

type t =
  | Integer of int
  | String of string
  | Boolean of bool
  | Float of float

let print c ppf =
  match c with
  | Integer k -> Print.print ppf "%d" k
  | String s -> Print.print ppf "%S" s
  | Boolean b -> Print.print ppf "%B" b
  | Float f -> Print.print ppf "%F" f

let of_integer n = Integer n
let of_string s = String s
let of_boolean b = Boolean b
let of_float f = Float f
