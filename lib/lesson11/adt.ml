(* {1 Type Definitions} *)

type typedef =
  | TTuple of typ list
  | TRecord of (string * typ) list
  | TVariant of (string * typ option) list
  | TAlias of string * typ

and typ =
  | TyBool
  | TyInt
  | TyString
  | TyUnit
  | TyTuple of typ list
  | TyRecord of (string * typ) list
  | TyVariant of (string * typ option) list
  | TyVar of string
  | TyArrow of typ * typ
  | TyName of string

(* {1 Expression and Value Definitions} *)

type value =
  | VBool of bool
  | VInt of int
  | VString of string
  | VUnit
  | VTuple of value list
  | VRecord of (string * value) list
  | VVariant of string * value option
  | VClosure of string * expr * value env

and expr =
  | EConst of value
  | EVar of string
  | ETuple of expr list
  | ERecord of (string * expr) list
  | EField of expr * string
  | EConstruct of string * expr option
  | EMatch of expr * match_rule list
  | EAbs of string * typ * expr
  | EApp of expr * expr
  | ELet of string * expr * expr

and match_rule = string list * expr

and 'a env = (string -> 'a option)

exception TypeError of string

(* {1 Type Constructors} *)

let arrow t1 t2 = TyArrow (t1, t2)

let tuple ts = TyTuple ts

let variant cases = TyVariant cases

let record fields = TyRecord fields

(* {1 Pretty Printing} *)

let rec string_of_typ = function
  | TyBool -> "Bool"
  | TyInt -> "Int"
  | TyString -> "String"
  | TyUnit -> "Unit"
  | TyVar s -> "'" ^ s
  | TyTuple [] -> "Unit"
  | TyTuple ts ->
      let elems = List.map string_of_typ ts in
      "(" ^ String.concat " * " elems ^ ")"
  | TyRecord fields ->
      let flds = List.map (fun (l, t) -> l ^ ":" ^ string_of_typ t) fields in
      "{" ^ String.concat "; " flds ^ "}"
  | TyVariant cases ->
      let cs = List.map (fun (c, ot) ->
        match ot with
        | None -> c
        | Some t -> c ^ " of " ^ string_of_typ t
      ) cases in
      "[|" ^ String.concat " | " cs ^ "|]"
  | TyArrow (t1, t2) ->
      let left = match t1 with
        | TyArrow _ -> "(" ^ string_of_typ t1 ^ ")"
        | _ -> string_of_typ t1
      in
      left ^ " → " ^ string_of_typ t2
  | TyName s -> s

let rec string_of_value = function
  | VBool b -> string_of_bool b
  | VInt n -> string_of_int n
  | VString s -> Printf.sprintf "\"%s\"" s
  | VUnit -> "()"
  | VTuple vs ->
      let elems = List.map string_of_value vs in
      "(" ^ String.concat ", " elems ^ ")"
  | VRecord flds ->
      let fs = List.map (fun (l, v) -> l ^ "=" ^ string_of_value v) flds in
      "{" ^ String.concat "; " fs ^ "}"
  | VVariant (c, None) -> c
  | VVariant (c, Some v) -> Printf.sprintf "%s(%s)" c (string_of_value v)
  | VClosure (x, _, _) -> "<λ" ^ x ^ ".>"

let rec string_of_expr = function
  | EConst v -> string_of_value v
  | EVar x -> x
  | ETuple es ->
      let elems = List.map string_of_expr es in
      "(" ^ String.concat ", " elems ^ ")"
  | ERecord flds ->
      let fs = List.map (fun (l, e) -> l ^ "=" ^ string_of_expr e) flds in
      "{" ^ String.concat "; " fs ^ "}"
  | EField (e, f) -> string_of_expr e ^ "." ^ f
  | EConstruct (c, None) -> c
  | EConstruct (c, Some e) -> Printf.sprintf "%s(%s)" c (string_of_expr e)
  | EMatch (e, _rules) ->
      "match " ^ string_of_expr e ^ " with ..."
  | EAbs (x, t, e) -> Printf.sprintf "λ%s:%s. %s" x (string_of_typ t) (string_of_expr e)
  | EApp (e1, e2) ->
      let left = match e1 with
        | EAbs _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      left ^ " " ^ string_of_expr e2
  | ELet (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_expr e1) (string_of_expr e2)

(* {1 Type Checking} *)

let rec typeof env = function
  | EConst (VBool _) -> TyBool
  | EConst (VInt _) -> TyInt
  | EConst (VString _) -> TyString
  | EConst VUnit -> TyUnit
  | EConst (VTuple _) -> TyUnit (* Placeholder *)
  | EConst (VRecord _) -> TyUnit (* Placeholder *)
  | EConst (VVariant _) -> TyUnit (* Placeholder *)
  | EConst (VClosure _) -> arrow TyUnit TyUnit (* Placeholder *)

  | EVar x ->
      (try env x |> Option.get
       with Not_found -> raise (TypeError ("Unbound: " ^ x)))

  | ETuple es ->
      let ts = List.map (typeof env) es in
      TyTuple ts

  | ERecord flds ->
      let ts = List.map (fun (_, e) -> typeof env e) flds in
      let labels = List.map fst flds in
      TyRecord (List.combine labels ts)

  | EField (e, f) ->
      (match typeof env e with
       | TyRecord fields ->
           (try List.assoc f fields
            with Not_found -> raise (TypeError ("No field: " ^ f)))
       | t -> raise (TypeError ("Expected record, got: " ^ string_of_typ t)))

  | EConstruct (_c, eo) ->
      (* For simplicity, assume we have type info from environment *)
      (* In real implementation, we'd look up the variant type *)
      (match eo with
       | None -> TyUnit (* Placeholder *)
       | Some e -> typeof env e)

  | EMatch (e, _) ->
      (* Pattern matching returns type of the first branch *)
      (* For simplicity, assume all branches have same type *)
      (* Real implementation would check all branches *)
      let _ = typeof env e in (* Check scrutinee *)
      TyUnit (* Placeholder - real impl would get from rules *)

  | EAbs (x, t1, e2) ->
      let env' y = if y = x then Some t1 else env y in
      let t2 = typeof env' e2 in
      TyArrow (t1, t2)

  | EApp (e1, e2) ->
      let t1 = typeof env e1 in
      let t2 = typeof env e2 in
      (match t1 with
       | TyArrow (t11, t12) ->
           if t2 = t11 then t12
           else raise (TypeError "Parameter type mismatch")
       | _ -> raise (TypeError "Expected arrow type"))

  | ELet (x, e1, e2) ->
      let t1 = typeof env e1 in
      let env' y = if y = x then Some t1 else env y in
      typeof env' e2

let check_type env e ty =
  let inferred = typeof env e in
  if inferred <> ty then
    raise (TypeError (Printf.sprintf "Expected %s, got %s"
      (string_of_typ ty) (string_of_typ inferred)))
