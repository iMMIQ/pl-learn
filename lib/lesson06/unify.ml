open Types

exception Unify_error of string * ty * ty

let rec unify t1 t2 =
  match t1, t2 with
  (* Same type - no substitution needed *)
  | TyVar v, TyVar w when v = w -> empty_subst

  (* Variable occurs check *)
  | TyVar v, _ ->
      if occurs_check v t2 then
        raise (Unify_error ("Occurs check failed", t1, t2))
      else
        single v t2

  | _, TyVar v ->
      if occurs_check v t1 then
        raise (Unify_error ("Occurs check failed", t1, t2))
      else
        single v t1

  (* Base types *)
  | TyBool, TyBool -> empty_subst
  | TyInt, TyInt -> empty_subst

  (* Function types *)
  | TyArrow (d1, c1), TyArrow (d2, c2) ->
      let s1 = unify d1 d2 in
      let s2 = unify (apply_subst s1 c1) (apply_subst s1 c2) in
      compose s1 s2

  (* Mismatch *)
  | _ ->
      raise (Unify_error ("Cannot unify", t1, t2))
