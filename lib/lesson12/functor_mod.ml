open Module_system

(* {1 Functor Type} *)

type functor_def = {
  fname : string;
  fparam : string * signature;
  fbody : structure;
}

(* {1 Structure Instantiation} *)

let rec instantiate_expr param_name struct_items = function
  | EConst n -> EConst n
  | EVar x -> EVar x  (* Would need to resolve module paths *)
  | ELam (x, t, e) -> ELam (x, t, instantiate_expr param_name struct_items e)
  | EApp (e1, e2) ->
      EApp (instantiate_expr param_name struct_items e1,
           instantiate_expr param_name struct_items e2)
  | EMatch (e, cases) ->
      EMatch (instantiate_expr param_name struct_items e,
              List.map (fun (p, e) -> (p, instantiate_expr param_name struct_items e)) cases)

let rec instantiate_structure param_name arg_items body_items =
  List.map (fun item ->
    match item with
    | StructType (name, ty) -> StructType (name, ty)
    | StructVal (name, e) ->
        StructVal (name, instantiate_expr param_name arg_items e)
    | StructModule (name, inner) ->
        StructModule (name, instantiate_structure param_name arg_items inner)
    | StructInclude inner ->
        StructInclude (instantiate_structure param_name arg_items inner)
  ) body_items

(* {1 Functor Application} *)

let apply_functor fct arg_structure =
  instantiate_structure (fst fct.fparam) arg_structure fct.fbody

(* {1 Functor Type Checking} *)

let check_functor fct =
  (* Check that the body is well-formed given the parameter signature *)
  let _, param_sig = fct.fparam in
  (* Build environment from parameter signature *)
  let rec build_env = function
    | [] -> (fun _ -> None)
    | SigType (name, _) :: rest ->
        let env_rest = build_env rest in
        (fun x -> if x = name then Some (TyName name) else env_rest x)
    | SigVal (name, ty) :: rest ->
        let env_rest = build_env rest in
        (fun x -> if x = name then Some ty else env_rest x)
    | SigTypedef (name, ty) :: rest ->
        let env_rest = build_env rest in
        (fun x -> if x = name then Some ty else env_rest x)
    | _ :: rest -> build_env rest
  in
  let env = build_env param_sig in
  (* Check body structure against extended signature *)
  typecheck_sig env param_sig  (* Simplified check *)

let check_functor_application fct arg_sig =
  let _, param_sig = fct.fparam in
  (* Check that arg_sig provides at least what param_sig requires *)
  List.iter (fun item ->
    match item with
    | SigType (name, _) ->
        if not (List.exists (function
            | SigType (n, _) -> n = name
            | SigTypedef (n, _) -> n = name
            | _ -> false) arg_sig) then
          raise (ModuleError ("Missing type: " ^ name))
    | SigVal (name, _) ->
        if not (List.exists (function
            | SigVal (n, _) -> n = name
            | _ -> false) arg_sig) then
          raise (ModuleError ("Missing value: " ^ name))
    | _ -> ()
  ) param_sig

(* {1 Higher-Order Functors} *)

let compose f1 f2 =
  {
    fname = f1.fname ^ "_compose_" ^ f2.fname;
    fparam = f2.fparam;
    fbody = (* Simplified - would properly compose *) f2.fbody;
  }

(* {1 Pretty Printing} *)

let string_of_functor f =
  Printf.sprintf "module %s (%s : sig ... end) : sig ... end"
    f.fname (fst f.fparam)
