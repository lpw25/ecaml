(** Built in constants *)

(** Type of constants. *)
type t = private
  | Integer of int
  | String of string
  | Boolean of bool

(** Print a constant. *)
val print : t -> Format.formatter -> unit

(** Create a constant from an integer. *)
val of_integer : int -> t

(** Create a constant from a string. *)
val of_string : string -> t

(** Create a constant from a boolean. *)
val of_boolean : bool -> t
