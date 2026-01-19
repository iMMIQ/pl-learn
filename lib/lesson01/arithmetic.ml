type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

let rec eval env = function
  | Const n -> n
  | Add (e1, e2) -> eval env e1 + eval env e2
  | Mul (e1, e2) -> eval env e1 * eval env e2
  | Var x ->
      (try env x
       with Not_found -> raise (Invalid_argument ("Unbound: " ^ x)))
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' y = if y = x then v1 else env y in
      eval env' e2

let run e = eval (fun _ -> raise Not_found) e

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Var x -> x
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let pp_expr fmt e =
  Format.pp_print_string fmt (string_of_expr e)
