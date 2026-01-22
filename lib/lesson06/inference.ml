open Types
open Unify

(* {1 Expressions} *)

type expr =
  | EConst of int
  | EBool of bool
  | EVar of string
  | EAbs of string * expr
  | EApp of expr * expr
  | ELet of string * expr * expr
  | EIf of expr * expr * expr
  | EBinOp of string * expr * expr

(* {1 Type Schemes} *)

type scheme =
  | Mono of ty
  | Poly of string list * ty

(* {1 Type Environment} *)

type env = (string * scheme) list

let env_lookup x env =
  try Some (List.assoc x env)
  with Not_found -> None

let env_extend x sc env = (x, sc) :: env

let rec freevars_ty = function
  | TyVar v -> [v]
  | TyBool | TyInt -> []
  | TyArrow (t1, t2) ->
      let f1 = freevars_ty t1 in
      let f2 = freevars_ty t2 in
      f1 @ f2 |> List.sort_uniq String.compare

let freevars_scheme = function
  | Mono t -> freevars_ty t
  | Poly (vars, t) ->
      freevars_ty t
      |> List.filter (fun v -> not (List.mem v vars))

let freevars_env env =
  List.fold_left (fun acc (_, sc) ->
    let fv = freevars_scheme sc in
    acc @ fv |> List.sort_uniq String.compare
  ) [] env

(* {1 Fresh Variable Generation} *)

let counter = ref 0

let fresh_var () =
  incr counter;
  "'t" ^ string_of_int !counter

let fresh () =
  TyVar (fresh_var ())

(* {1 Generalization} *)

let generalize env ty =
  let env_vars = freevars_env env in
  let ty_vars = freevars_ty ty in
  let poly_vars = List.filter (fun v -> not (List.mem v env_vars)) ty_vars in
  match poly_vars with
  | [] -> Mono ty
  | _ -> Poly (poly_vars, ty)

(* {1 Instantiation} *)

let instantiate = function
  | Mono t -> t
  | Poly (vars, t) ->
      let subst = List.map (fun v -> (v, fresh ())) vars in
      apply_subst subst t

(* {1 Type Inference} *)

exception TypeError of string

let rec infer env = function
  | EConst _ ->
      (TyInt, empty_subst)

  | EBool _ ->
      (TyBool, empty_subst)

  | EVar x ->
      (match env_lookup x env with
       | None -> raise (TypeError ("Unbound variable: " ^ x))
       | Some sc ->
           let ty = instantiate sc in
           (ty, empty_subst))

  | EAbs (x, e) ->
      (* Create fresh type variable for argument *)
      let tx = fresh () in
      let env' = env_extend x (Mono tx) env in
      let (tbody, s) = infer env' e in
      (* Result: tx -> tbody *)
      (TyArrow (apply_subst s tx, tbody), s)

  | EApp (e1, e2) ->
      let (ty1, s1) = infer env e1 in
      let (ty2, s2) = infer (apply_env_subst s1 env) e2 in
      (* Create fresh result type *)
      let tr = fresh () in
      (* Unify: ty1 = ty2 -> tr *)
      let s3 = unify (apply_subst s2 ty1)
                     (TyArrow (apply_subst s2 ty2, tr)) in
      (* Compose substitutions *)
      let s = compose s2 (compose s1 s3) in
      (apply_subst s tr, s)

  | ELet (x, e1, e2) ->
      let (ty1, s1) = infer env e1 in
      let env' = apply_env_subst s1 env in
      let sc1 = generalize env' ty1 in
      let env'' = env_extend x sc1 env' in
      let (ty2, s2) = infer env'' e2 in
      (ty2, compose s1 s2)

  | EIf (e1, e2, e3) ->
      let (ty1, s1) = infer env e1 in
      let s2 = unify (apply_subst s1 ty1) TyBool in
      let env' = apply_env_subst (compose s1 s2) env in
      let (ty2, s3) = infer env' e2 in
      let (ty3, s4) = infer (apply_env_subst s3 env') e3 in
      let s5 = unify (apply_subst s4 ty2) (apply_subst s4 ty3) in
      let s = compose s4 (compose s3 (compose s2 (compose s1 s5))) in
      (apply_subst s ty2, s)

  | EBinOp (op, e1, e2) ->
      let (ty1, s1) = infer env e1 in
      let (ty2, s2) = infer (apply_env_subst s1 env) e2 in
      let (ty_res, s3) = match op with
        | "+" | "-" | "*" | "/" ->
            let s = unify (apply_subst s2 ty1) TyInt in
            let s' = unify (apply_subst s (apply_subst s2 ty2)) TyInt in
            (TyInt, compose s s')
        | "=" | "<" | ">" ->
            let s = unify (apply_subst s2 ty1) TyInt in
            let s' = unify (apply_subst s (apply_subst s2 ty2)) TyInt in
            (TyBool, compose s s')
        | _ -> raise (TypeError ("Unknown operator: " ^ op))
      in
      (ty_res, compose s2 (compose s1 s3))

and apply_env_subst s env =
  List.map (fun (x, sc) ->
    let sc' = match sc with
      | Mono t -> Mono (apply_subst s t)
      | Poly (vars, t) -> Poly (vars, apply_subst s t)
    in
    (x, sc')
  ) env

let typeof e =
  fst (infer [] e)

(* {1 Pretty Printing} *)

let rec string_of_expr = function
  | EConst n -> string_of_int n
  | EBool b -> string_of_bool b
  | EVar x -> x
  | EAbs (x, e) -> "λ" ^ x ^ ". " ^ string_of_expr e
  | EApp (e1, e2) ->
      let left = match e1 with
        | EAbs _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      left ^ " " ^ string_of_expr e2
  | ELet (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | EIf (e1, e2, e3) ->
      "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^
      " else " ^ string_of_expr e3
  | EBinOp (op, e1, e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ op ^ " " ^ string_of_expr e2 ^ ")"

let string_of_scheme = function
  | Mono t -> string_of_ty t
  | Poly ([], t) -> string_of_ty t
  | Poly (vars, t) ->
      "∀" ^ String.concat " " vars ^ ". " ^ string_of_ty t
