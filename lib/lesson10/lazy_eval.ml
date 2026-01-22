(* {1 Expression Syntax} *)

type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr
  | Const of int
  | Prim of string * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

(* {1 Values} *)

type value =
  | VConst of int
  | VClosure of string * expr * env
  | VThunk of expr * env

and env = (string -> value option)

(* {1 Environment Operations} *)

let empty_env _ = None

let extend env x v y =
  if y = x then Some v else env y

(* {1 Call-by-Name Evaluation} *)

let rec eval env = function
  | Var x ->
      (try env x |> Option.get
       with Not_found -> failwith ("Unbound: " ^ x))

  | Const n -> VConst n

  | Abs (x, body) -> VClosure (x, body, env)

  | App (e1, e2) ->
      (* Call-by-name: don't evaluate e2! *)
      let f = eval env e1 in
      (match f with
       | VClosure (x, body, closure_env) ->
           (* Bind x to a thunk containing e2 *)
           let thunk = VThunk (e2, env) in
           eval (extend closure_env x thunk) body
       | _ -> failwith "Cannot apply non-function")

  | Prim (op, e1, e2) ->
      (* Need to force evaluation for primitives *)
      let v1 = force_val (eval env e1) in
      let v2 = force_val (eval env e2) in
      (match v1, v2 with
       | VConst n1, VConst n2 ->
           (match op with
            | "+" -> VConst (n1 + n2)
            | "*" -> VConst (n1 * n2)
            | "-" -> VConst (n1 - n2)
            | _ -> failwith ("Unknown op: " ^ op))
       | _ -> failwith "Type error in primitive")

  | If (e1, e2, e3) ->
      let v1 = force_val (eval env e1) in
      (match v1 with
       | VConst 0 -> eval env e3
       | VConst _ -> eval env e2
       | _ -> failwith "If condition must be constant")

  | Let (x, e1, e2) ->
      (* Bind x to thunk (not evaluated) *)
      let thunk = VThunk (e1, env) in
      eval (extend env x thunk) e2

(* {1 Forcing Values} *)

and force_val = function
  | VConst n -> VConst n
  | VClosure _ as v -> v
  | VThunk (e, env) ->
      (* Evaluate the thunk *)
      eval env e

(* {1 Pretty Printing} *)

let rec string_of_expr = function
  | Var x -> x
  | Abs (x, e) -> "λ" ^ x ^ ". " ^ string_of_expr e
  | App (e1, e2) ->
      let left = match e1 with
        | Abs _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      left ^ " " ^ string_of_expr e2
  | Const n -> string_of_int n
  | Prim (op, e1, e2) ->
      Printf.sprintf "%s(%s, %s)" op (string_of_expr e1) (string_of_expr e2)
  | If (e1, e2, e3) ->
      Printf.sprintf "if %s then %s else %s"
        (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Let (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_expr e1) (string_of_expr e2)

let string_of_value = function
  | VConst n -> string_of_int n
  | VClosure (x, _, _) -> "<λ" ^ x ^ ". ...>"
  | VThunk _ -> "<thunk>"
