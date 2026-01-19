type typ =
  | TyBool
  | TyNat
  | TyArrow of typ * typ

type expr =
  | TmVar of string * typ
  | TmAbs of string * typ * expr
  | TmApp of expr * expr
  | TmTrue
  | TmFalse
  | TmIf of expr * expr * expr
  | TmZero
  | TSucc of expr
  | TPred of expr
  | TmIsZero of expr

exception TypeError of string

(* {1 Type Checking} *)

let rec typeof env = function
  | TmVar (x, _) ->
      (try env x |> Option.get
       with Not_found -> raise (TypeError ("Unbound: " ^ x)))

  | TmAbs (x, ty1, e2) ->
      let env' y = if y = x then Some ty1 else env y in
      let ty2 = typeof env' e2 in
      TyArrow (ty1, ty2)

  | TmApp (e1, e2) ->
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      (match ty1 with
       | TyArrow (ty11, ty12) ->
           if ty2 = ty11 then ty12
           else raise (TypeError "Parameter type mismatch")
       | _ -> raise (TypeError "Expected arrow type"))

  | TmTrue -> TyBool
  | TmFalse -> TyBool

  | TmIf (e1, e2, e3) ->
      if typeof env e1 <> TyBool then
        raise (TypeError "if condition must be bool");
      let ty2 = typeof env e2 in
      let ty3 = typeof env e3 in
      if ty2 = ty3 then ty2
      else raise (TypeError "Branches must have same type")

  | TmZero -> TyNat

  | TSucc e ->
      if typeof env e = TyNat then TyNat
      else raise (TypeError "succ expects nat")

  | TPred e ->
      if typeof env e = TyNat then TyNat
      else raise (TypeError "pred expects nat")

  | TmIsZero e ->
      if typeof env e = TyNat then TyBool
      else raise (TypeError "iszero expects nat")

let typecheck e = typeof (fun _ -> None) e

(* {1 Evaluation} *)

let rec eval = function
  | TmApp (TmAbs (_, _, body), arg) ->
      eval (subst 0 arg body)
  | TmApp (e1, e2) ->
      TmApp (eval e1, eval e2)
  | TmIf (TmTrue, e2, _) -> eval e2
  | TmIf (TmFalse, _, e3) -> eval e3
  | TmIf (e1, e2, e3) -> TmIf (eval e1, e2, e3)
  | TSucc e -> TSucc (eval e)
  | TPred e -> TPred (eval e)
  | TmIsZero e -> TmIsZero (eval e)
  | e -> e

and subst i value = function
  | TmVar (x, _) when x = "_" && i = 0 -> value
  | TmVar (x, ty) -> TmVar (x, ty)
  | TmAbs (x, ty, body) ->
      TmAbs (x, ty, if x = "_" then subst (i + 1) value body else body)
  | TmApp (e1, e2) -> TmApp (subst i value e1, subst i value e2)
  | TmTrue -> TmTrue
  | TmFalse -> TmFalse
  | TmIf (e1, e2, e3) -> TmIf (subst i value e1, subst i value e2, subst i value e3)
  | TmZero -> TmZero
  | TSucc e -> TSucc (subst i value e)
  | TPred e -> TPred (subst i value e)
  | TmIsZero e -> TmIsZero (subst i value e)

(* {1 Pretty Printing} *)

let rec string_of_typ = function
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | TyArrow (t1, t2) ->
      let left = match t1 with
        | TyArrow _ -> "(" ^ string_of_typ t1 ^ ")"
        | _ -> string_of_typ t1
      in
      left ^ " → " ^ string_of_typ t2

let rec string_of_expr = function
  | TmVar (x, _) -> x
  | TmAbs (x, ty, e) ->
      "λ" ^ x ^ ":" ^ string_of_typ ty ^ ". " ^ string_of_expr e
  | TmApp (e1, e2) ->
      let left = match e1 with
        | TmAbs _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      left ^ " " ^ string_of_expr e2
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^
      " else " ^ string_of_expr e3
  | TmZero -> "0"
  | TSucc e -> "succ " ^ string_of_expr e
  | TPred e -> "pred " ^ string_of_expr e
  | TmIsZero e -> "iszero " ^ string_of_expr e
