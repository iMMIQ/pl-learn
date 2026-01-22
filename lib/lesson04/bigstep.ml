(* Reuse expression type from smallstep - in practice, share via common module *)
type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

type derivation =
  | DConst of int
  | DAdd of derivation * derivation * int
  | DMul of derivation * derivation * int
  | DVar of string * int
  | DLet of string * derivation * derivation * int

let rec eval env = function
  | Const n -> n
  | Add (e1, e2) -> eval env e1 + eval env e2
  | Mul (e1, e2) -> eval env e1 * eval env e2
  | Var x ->
      (try env x with Not_found ->
         raise (Invalid_argument ("Unbound: " ^ x)))
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' y = if y = x then v1 else env y in
      eval env' e2

let rec eval_derivation env = function
  | Const n -> (n, DConst n)
  | Add (e1, e2) ->
      let (v1, d1) = eval_derivation env e1 in
      let (v2, d2) = eval_derivation env e2 in
      (v1 + v2, DAdd (d1, d2, v1 + v2))
  | Mul (e1, e2) ->
      let (v1, d1) = eval_derivation env e1 in
      let (v2, d2) = eval_derivation env e2 in
      (v1 * v2, DMul (d1, d2, v1 * v2))
  | Var x ->
      let v = env x in
      (v, DVar (x, v))
  | Let (x, e1, e2) ->
      let (v1, d1) = eval_derivation env e1 in
      let env' y = if y = x then v1 else env y in
      let (v2, d2) = eval_derivation env' e2 in
      (v2, DLet (x, d1, d2, v2))

let rec string_of_derivation indent = function
  | DConst n ->
      indent ^ "── Const(" ^ string_of_int n ^ ") ⇓ " ^ string_of_int n
  | DVar (x, v) ->
      indent ^ "── Var(" ^ x ^ ") ⇓ " ^ string_of_int v
  | DAdd (d1, d2, v) ->
      indent ^ "── Add(" ^ string_of_int v ^ ")\n" ^
      string_of_derivation (indent ^ "  ") d1 ^ "\n" ^
      string_of_derivation (indent ^ "  ") d2 ^ "\n" ^
      indent ^ "── ⇓ " ^ string_of_int v
  | DMul (d1, d2, v) ->
      indent ^ "── Mul(" ^ string_of_int v ^ ")\n" ^
      string_of_derivation (indent ^ "  ") d1 ^ "\n" ^
      string_of_derivation (indent ^ "  ") d2 ^ "\n" ^
      indent ^ "── ⇓ " ^ string_of_int v
  | DLet (x, d1, d2, v) ->
      indent ^ "── Let(" ^ x ^ ", " ^ string_of_int v ^ ")\n" ^
      string_of_derivation (indent ^ "  ") d1 ^ "\n" ^
      string_of_derivation (indent ^ "  ") d2 ^ "\n" ^
      indent ^ "── ⇓ " ^ string_of_int v

let pp_derivation fmt d =
  Format.pp_print_string fmt (string_of_derivation "" d)

let string_of_derivation = string_of_derivation ""
