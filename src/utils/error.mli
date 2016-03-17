(** Error reporting

    All internal errors are represented uniformly with a single exception that
    carries additional details such as error kind (syntax, typing, ...), message
    or location.

    Errors are raised through helper functions that take an optional location
    and a message in form of a format string, for example:
    [Error.runtime ~loc "Unhandled effect %t" (Effect.print eff)]. *)

(** Type of errors. *)
type t

(** Print an error. *)
val print : t -> unit

(** Exception representing all possible errors. *)
exception Error of t

(** Fatal errors. *)
val fatal : ?loc:Location.t -> ('a, Format. formatter, unit, 'b) format4 -> 'a

(** Syntax errors. *)
val syntax : loc:Location.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Typing errors. *)
val typing : loc:Location.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Runtime errors. *)
val runtime : ?loc:Location.t -> ('a, Format.formatter, unit, 'b) format4 -> 'a
