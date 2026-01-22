(* {1 Types} *)

type ty =
  | TyVar of string
  | TyBool
  | TyInt
  | TyArrow of ty * ty

(* {1 Substitutions} *)

type substitution = (string * ty) list

let empty_subst = []

let single v ty = [(v, ty)]

let dom s = List.map fst s

let rec occurs_check v = function
  | TyVar w -> v = w
  | TyBool | TyInt -> false
  | TyArrow (t1, t2) -> occurs_check v t1 || occurs_check v t2

let rec apply_subst s = function
  | TyVar v ->
      (try List.assoc v s
       with Not_found -> TyVar v)
  | TyBool -> TyBool
  | TyInt -> TyInt
  | TyArrow (t1, t2) ->
      TyArrow (apply_subst s t1, apply_subst s t2)

let compose s1 s2 =
  (* s1 ∘ s2: apply s1 to all types in s2, then prepend *)
  let s2' = List.map (fun (v, t) -> (v, apply_subst s1 t)) s2 in
  s2' @ s1

(* {1 Pretty Printing} *)

let rec string_of_ty = function
  | TyVar v -> "'" ^ v
  | TyBool -> "Bool"
  | TyInt -> "Int"
  | TyArrow (t1, t2) ->
      let left = match t1 with
        | TyArrow _ -> "(" ^ string_of_ty t1 ^ ")"
        | _ -> string_of_ty t1
      in
      left ^ " -> " ^ string_of_ty t2

let string_of_subst s =
  if s = [] then "[]"
  else
    let bindings = List.map (fun (v, t) ->
      "'" ^ v ^ " := " ^ string_of_ty t
    ) s in
    "[" ^ String.concat "; " bindings ^ "]"
