(* {1 Source Language} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(* {1 CPS Language} *)

(* In CPS, all function calls are tail calls.
 * A CPS expression either:
 * - Returns a value directly (Const, Var)
 * - Makes a tail call (CPrim)
 * - Binds a value and continues (CLet)
 *)
type cps_expr =
  | CConst of int
  | CVar of string
  | CPrim of string * cps_expr list
  | CLet of string * cps_expr * cps_expr

(* {1 Fresh Variable Generation} *)

let counter = ref 0
let fresh prefix =
  incr counter;
  prefix ^ "_" ^ string_of_int !counter

(* {1 CPS Transformation} *)

(* CPS transforms:
 *   [[n]]         = halt n
 *   [[x]]         = halt x
 *   [[e1 + e2]]   = let x1 = [[e1]] in
 *                   let x2 = [[e2]] in
 *                   halt (x1 + x2)
 *   [[let x = e1 in e2]]
 *                 = let x1 = [[e1]] in [[e2[x := x1]]]
 *
 * where "halt e" means "return value e"
 *)

let cps_transform e =
  let rec transform e =
    match e with
    | Const n -> CConst n

    | Var x -> CVar x

    | Add (e1, e2) ->
        let x1 = fresh "x" in
        let x2 = fresh "x" in
        let e1_cps = transform e1 in
        let e2_cps = transform e2 in
        (* let x1 = e1_cps in let x2 = e2_cps in +(x1, x2) *)
        CLet (x1, e1_cps,
          CLet (x2, e2_cps,
            CPrim ("+", [CVar x1; CVar x2])))

    | Mul (e1, e2) ->
        let x1 = fresh "x" in
        let x2 = fresh "x" in
        let e1_cps = transform e1 in
        let e2_cps = transform e2 in
        CLet (x1, e1_cps,
          CLet (x2, e2_cps,
            CPrim ("*", [CVar x1; CVar x2])))

    | Let (x, e1, e2) ->
        let e1_cps = transform e1 in
        let e2_cps = transform e2 in
        CLet (x, e1_cps, e2_cps)

  in
  counter := 0;
  transform e

(* {1 CPS Evaluation} *)

let rec eval_cps env = function
  | CConst n -> n
  | CVar x ->
      (try env x with Not_found ->
         raise (Invalid_argument ("Unbound: " ^ x)))
  | CPrim (op, args) ->
      let values = List.map (eval_cps env) args in
      (match op, values with
       | "+", [n1; n2] -> n1 + n2
       | "*", [n1; n2] -> n1 * n2
       | _ -> raise (Invalid_argument ("Unknown op: " ^ op)))
  | CLet (x, e1, e2) ->
      let v1 = eval_cps env e1 in
      let env' y = if y = x then v1 else env y in
      eval_cps env' e2

(* {1 Pretty Printing} *)

let rec string_of_cps_expr = function
  | CConst n -> string_of_int n
  | CVar x -> x
  | CPrim (op, args) ->
      let args_str = String.concat ", " (List.map string_of_cps_expr args) in
      Printf.sprintf "%s(%s)" op args_str
  | CLet (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_cps_expr e1) (string_of_cps_expr e2)
