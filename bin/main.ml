type term = False | True

type expr =
  | And of (expr * expr)
  | Or of (expr * expr)
  | Not of expr
  | Atom of term

let str_of_term = function
  | False -> "false"
  | True -> "true"

let rec and_rules (a, b) = match (dummy a, dummy b) with
  | (True, True) -> True
  | _ -> False

and or_rules (a, b) = match (dummy a, dummy b) with
  | False, False -> False
  | _ -> True

and not expr = match dummy expr with
  | True -> False
  | False -> True

and dummy expr = match expr with
  | And args -> and_rules args
  | Or args -> or_rules args
  | Not e -> not e
  | Atom t -> t

let example = Not (Or (Atom(False), And (Atom(True), Not(Atom(False)))))

let () =
  let reduced = dummy example in
  print_endline (str_of_term reduced)
