open Subtype

(* {1 Types} *)

type ty = Subtype.ty =
  | TyTop
  | TyBot
  | TyBool
  | TyInt
  | TyString
  | TyRecord of (string * ty) list
  | TyArrow of ty * ty

(* {1 Expressions} *)

type expr =
  | EConst of int
  | EBool of bool
  | EString of string
  | EVar of string
  | EAbs of string * ty * expr
  | EApp of expr * expr
  | ERecord of (string * expr) list
  | EProj of expr * string
  | ELet of string * expr * expr

(* {1 Type Environment} *)

type env = (string * ty) list

let env_extend x ty env = (x, ty) :: env
let env_lookup x env =
  try Some (List.assoc x env)
  with Not_found -> None

(* {1 Subsumption} *)

let subsumption ty expected =
  if is_subtype ty expected then expected
  else raise (SubtypeError (
    Printf.sprintf "Type %s is not a subtype of %s"
      (string_of_ty ty) (string_of_ty expected)
  ))

(* {1 Type Checking} *)

let rec typeof env = function
  | EConst _ -> TyInt
  | EBool _ -> TyBool
  | EString _ -> TyString

  | EVar x ->
      (match env_lookup x env with
       | Some ty -> ty
       | None -> raise (SubtypeError ("Unbound: " ^ x)))

  | EAbs (x, ty1, e) ->
      let env' = env_extend x ty1 env in
      let ty2 = typeof env' e in
      TyArrow (ty1, ty2)

  | EApp (e1, e2) ->
      let ty1 = typeof env e1 in
      let ty2 = typeof env e2 in
      (match ty1 with
       | TyArrow (ty1_domain, ty1_codomain) ->
           if is_subtype ty2 ty1_domain then
             ty1_codomain
           else
             raise (SubtypeError (
               Printf.sprintf "Argument type %s not subtype of %s"
                 (string_of_ty ty2) (string_of_ty ty1_domain)
             ))
       | _ -> raise (SubtypeError "Expected function type"))

  | ERecord fields ->
      let field_tys = List.map (fun (l, e) -> (l, typeof env e)) fields in
      TyRecord field_tys

  | EProj (e, l) ->
      (match typeof env e with
       | TyRecord fields ->
           (try List.assoc l fields
            with Not_found ->
              raise (SubtypeError ("No field " ^ l ^ " in record")))
       | ty -> raise (SubtypeError (
           Printf.sprintf "Cannot project from non-record type %s"
             (string_of_ty ty))))

  | ELet (x, e1, e2) ->
      let ty1 = typeof env e1 in
      let env' = env_extend x ty1 env in
      typeof env' e2

let typecheck e = typeof [] e

(* {1 Pretty Printing} *)

let rec string_of_expr = function
  | EConst n -> string_of_int n
  | EBool b -> string_of_bool b
  | EString s -> Printf.sprintf "\"%s\"" s
  | EVar x -> x
  | EAbs (x, ty, e) ->
      "λ" ^ x ^ ":" ^ string_of_ty ty ^ ". " ^ string_of_expr e
  | EApp (e1, e2) ->
      let left = match e1 with
        | EAbs _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      left ^ " " ^ string_of_expr e2
  | ERecord fields ->
      let fields_str = String.concat ", " (List.map (fun (l, e) ->
        l ^ " = " ^ string_of_expr e
      ) fields) in
      "{ " ^ fields_str ^ " }"
  | EProj (e, l) -> string_of_expr e ^ "." ^ l
  | ELet (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
