(* {1 Source Language} *)

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

(* {1 CPS Language} *)

(* A CPS expression represents a computation in tail position.
 * Every value is produced and immediately passed to a continuation. *)
type cps_expr =
  | CConst of int
  | CVar of string
  | CPrim of string * cps_expr list * cps_cont
  | CLet of string * cps_expr * cps_tail
  | CApp of cps_expr * cps_expr list

(* A continuation represents "what to do with a value" *)
and cps_cont =
  | CId
  | CArg of string * cps_cont
  | CBin1 of string * cps_expr * cps_cont
  | CFunc of cps_expr * cps_cont

(* A tail is what comes after a let-binding - it produces the final value *)
and cps_tail =
  | CTail of cps_expr
  | CCont of cps_cont

(* {1 Fresh Variable Generation} *)

let counter = ref 0
let fresh prefix =
  incr counter;
  prefix ^ "_" ^ string_of_int !counter

(* {1 CPS Transformation} *)

let rec cps_transform e =
  let rec transform e k =
    match e with
    | Const n -> apply_cont k (CConst n)

    | Var x -> apply_cont k (CVar x)

    | Add (e1, e2) ->
        (* For Add(e1, e2):
           1. Transform e1 to get value v1
           2. Transform e2 to get value v2
           3. Compute v1 + v2
           4. Pass result to k
        *)
        let x1 = fresh "x" in
        let x2 = fresh "x" in
        (* First, transform e1 with continuation that handles e2 and add *)
        let e2_cps = cps_transform e2 in
        let kont = CBin1 ("+", e2_cps, CArg (x2, k)) in
        let e1_cps = transform e1 kont in
        (* Wrap in CLet: evaluate e1_cps, bind to x1, then do the rest *)
        CLet (x1, e1_cps, CCont (CBin1 ("+", CVar x2, CArg (x2, k))))

    | Mul (e1, e2) ->
        let x1 = fresh "x" in
        let x2 = fresh "x" in
        let e2_cps = cps_transform e2 in
        let kont = CBin1 ("*", e2_cps, CArg (x2, k)) in
        let e1_cps = transform e1 kont in
        CLet (x1, e1_cps, CCont (CBin1 ("*", CVar x2, CArg (x2, k))))

    | Let (x, e1, e2) ->
        let e2_cps = cps_transform e2 in
        transform e1 (CFunc (e2_cps, CArg (x, k)))

  and apply_cont k v =
    match k with
    | CId -> v
    | CArg (x, k') -> CLet (x, v, CTail (apply_cont k' (CVar x)))
    | CBin1 (op, e2, k') -> CPrim (op, [v; e2], k')
    | CFunc (f, _k') -> CApp (f, [v])

  in
  counter := 0;
  transform e CId

(* {1 CPS Evaluation} *)

let rec eval_cps env = function
  | CConst n -> n
  | CVar x ->
      (try env x with Not_found ->
         raise (Invalid_argument ("Unbound: " ^ x)))
  | CPrim (op, args, k) ->
      let values = List.map (eval_cps env) args in
      let result = match op, values with
        | "+", [n1; n2] -> n1 + n2
        | "*", [n1; n2] -> n1 * n2
        | _ -> raise (Invalid_argument ("Unknown op: " ^ op))
      in
      eval_cont env k result

  | CLet (x, e1, tail) ->
      let v1 = eval_cps env e1 in
      let env' y = if y = x then v1 else env y in
      eval_tail env' tail

  | CApp (f, _args) ->
      eval_cps env f

and eval_tail env = function
  | CTail e -> eval_cps env e
  | CCont k ->
      (* Need a value to pass to continuation - use 0 as dummy *)
      eval_cont env k 0

and eval_cont env k v =
  match k with
  | CId -> v
  | CArg (x, k') ->
      let env' y = if y = x then v else env y in
      eval_cont env' k' v
  | CBin1 (op, e2, k') ->
      let v2 = eval_cps env e2 in
      let result = match op with
        | "+" -> v + v2
        | "*" -> v * v2
        | _ -> raise (Invalid_argument "Unknown op")
      in
      eval_cont env k' result
  | CFunc (f, k') ->
      eval_cont env k' (eval_cps env f)

(* {1 Pretty Printing} *)

let rec string_of_cps_expr = function
  | CConst n -> string_of_int n
  | CVar x -> x
  | CPrim (op, args, k) ->
      let args_str = String.concat ", " (List.map string_of_cps_expr args) in
      Printf.sprintf "%s(%s); %s" op args_str (string_of_cps_cont k)
  | CLet (x, e1, tail) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_cps_expr e1) (string_of_cps_tail tail)
  | CApp (f, args) ->
      let args_str = String.concat ", " (List.map string_of_cps_expr args) in
      Printf.sprintf "%s(%s)" (string_of_cps_expr f) args_str

and string_of_cps_tail = function
  | CTail e -> string_of_cps_expr e
  | CCont k -> string_of_cps_cont k

and string_of_cps_cont = function
  | CId -> "halt"
  | CArg (x, k) -> Printf.sprintf "(%s -> %s)" x (string_of_cps_cont k)
  | CBin1 (op, e2, k) -> Printf.sprintf "(<%s> %s; %s)" op (string_of_cps_expr e2) (string_of_cps_cont k)
  | CFunc (f, k) -> Printf.sprintf "(<app> %s; %s)" (string_of_cps_expr f) (string_of_cps_cont k)
