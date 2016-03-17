(** Pretty-printing functions *)

let message ?loc ~header fmt =
  match loc with
  | None -> Format.eprintf ("%s:@," ^^ fmt ^^ "@.") header
  | Some loc -> Format.eprintf ("%s (%t):@," ^^ fmt ^^ "@.") header (Location.print loc)

let print ?(at_level=min_int) ?(max_level=max_int) ppf =
  if at_level <= max_level then
    Format.fprintf ppf
  else
    fun fmt -> Format.fprintf ppf ("(" ^^ fmt ^^ ")")

let rec sequence sep pp vs ppf =
  match vs with
  | [] -> ()
  | [v] -> pp v ppf
  | v :: vs -> Format.fprintf ppf "%t%s@,%t" (pp v) sep (sequence sep pp vs)
