(* {1 Signature and Structure Types} *)

type typ =
  | TyInt
  | TyBool
  | TyVar of string
  | TyArrow of typ * typ
  | TyName of string

and expr =
  | EConst of int
  | EVar of string
  | ELam of string * typ * expr
  | EApp of expr * expr
  | EMatch of expr * (pattern * expr) list

and pattern =
  | PVar of string
  | PConst of int
  | PWildcard

type sig_item =
  | SigType of string * kind option
  | SigTypedef of string * typ
  | SigVal of string * typ
  | SigModule of string * signature
  | SigInclude of signature

and signature = sig_item list

and kind =
  | KStar
  | KArrow of kind * kind

type struct_item =
  | StructType of string * typ option
  | StructVal of string * expr
  | StructModule of string * structure
  | StructInclude of structure

and structure = struct_item list

type module_value =
  | MVStructure of structure
  | MVFunctor of (string * signature) * (structure -> structure)

exception ModuleError of string

(* {1 Signature Operations} *)

let empty_sig = []

let add_type name sig_items =
  SigType (name, None) :: sig_items

let add_val name ty sig_items =
  SigVal (name, ty) :: sig_items

let extend sig1 sig2 =
  sig1 @ sig2

(* {1 Structure Operations} *)

let empty_struct = []

let add_value name expr struct_items =
  StructVal (name, expr) :: struct_items

let add_type_binding name ty struct_items =
  StructType (name, ty) :: struct_items

(* {1 Type Checking} *)

let rec string_of_typ = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVar s -> "'" ^ s
  | TyArrow (t1, t2) ->
      let left = match t1 with
        | TyArrow _ -> "(" ^ string_of_typ t1 ^ ")"
        | _ -> string_of_typ t1
      in
      left ^ " -> " ^ string_of_typ t2
  | TyName s -> s

let rec string_of_kind = function
  | KStar -> "*"
  | KArrow (k1, k2) ->
      let left = match k1 with
        | KArrow _ -> "(" ^ string_of_kind k1 ^ ")"
        | _ -> string_of_kind k1
      in
      left ^ " -> " ^ string_of_kind k2

let string_of_sig_item = function
  | SigType (name, None) -> "type " ^ name
  | SigType (name, Some k) -> "type " ^ name ^ " : " ^ string_of_kind k
  | SigTypedef (name, ty) -> "type " ^ name ^ " = " ^ string_of_typ ty
  | SigVal (name, ty) -> "val " ^ name ^ " : " ^ string_of_typ ty
  | SigModule (name, _) ->
      "module " ^ name ^ " : " ^ "{...}"
  | SigInclude _ -> "include ..."

let string_of_signature sig_items =
  let items = List.map string_of_sig_item sig_items in
  "{\n  " ^ String.concat "\n  " items ^ "\n}"

let string_of_struct_item = function
  | StructType (name, None) -> "type " ^ name
  | StructType (name, Some ty) -> "type " ^ name ^ " = " ^ string_of_typ ty
  | StructVal (name, _) -> "val " ^ name ^ " = ..."
  | StructModule (name, _) -> "module " ^ name ^ " = ..."
  | StructInclude _ -> "include ..."

let string_of_structure struct_items =
  let items = List.map string_of_struct_item struct_items in
  "{\n  " ^ String.concat "\n  " items ^ "\n}"

(* {1 Signature Checking} *)

let rec check_item_against_sig item = function
  | [] -> raise (ModuleError "Item not in signature")
  | SigType (name, _) :: rest when type_matches_item name item -> rest
  | SigVal (name, _) :: rest when value_matches_item name item -> rest
  | _ :: rest -> check_item_against_sig item rest

and type_matches_item name = function
  | StructType (n, _) -> n = name
  | _ -> false

and value_matches_item name = function
  | StructVal (n, _) -> n = name
  | _ -> false

let check_structure sig_items struct_items =
  (* Check that all structure items are allowed by signature *)
  List.iter (fun item ->
    try ignore (check_item_against_sig item sig_items)
    with ModuleError msg ->
      raise (ModuleError ("Signature check failed: " ^ msg))
  ) struct_items;

  (* Check that all required signature items are provided *)
  let rec check_required sig_items provided = match sig_items with
    | [] -> ()
    | SigType (name, _) :: rest ->
        if List.exists (type_matches_item name) provided
        then check_required rest provided
        else ()  (* Abstract type can be provided by implementation *)
    | SigVal (name, _) :: rest ->
        if List.exists (value_matches_item name) provided
        then check_required rest provided
        else raise (ModuleError ("Missing value: " ^ name))
    | _ :: rest -> check_required rest provided
  in
  check_required sig_items struct_items

let rec typecheck_sig env = function
  | [] -> ()
  | SigType (_, _) :: rest -> typecheck_sig env rest
  | SigTypedef (_, ty) :: rest ->
      (* Check type expression is well-formed *)
      typecheck_typ env ty;
      typecheck_sig env rest
  | SigVal (_, ty) :: rest ->
      (* Check type expression is well-formed *)
      typecheck_typ env ty;
      typecheck_sig env rest
  | SigInclude sig_items :: rest ->
      typecheck_sig env sig_items;
      typecheck_sig env rest
  | SigModule _ :: rest -> typecheck_sig env rest

and typecheck_typ env = function
  | TyInt | TyBool -> ()
  | TyVar x ->
      if env x = None then
        raise (ModuleError ("Unbound type variable: " ^ x))
  | TyArrow (t1, t2) ->
      typecheck_typ env t1;
      typecheck_typ env t2
  | TyName _ -> ()  (* Would check in full implementation *)
