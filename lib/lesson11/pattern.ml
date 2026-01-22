open Adt

(* {1 Pattern Type} *)

type pattern =
  | PWildcard
  | PVar of string
  | PConst of value
  | PTuple of pattern list
  | PRecord of (string * pattern) list
  | PVariant of string * pattern option
  | POr of pattern * pattern
  | PAs of string * pattern

(* {1 Internal Type for Exhaustiveness Checking} *)

and typ =
  | TBool
  | TInt
  | TUnit
  | TVariant of (string * typ option) list
  | TTuple of typ list

(* {1 Decision Tree Type} *)

and decision_tree =
  | DLeaf of string list  (* Variable names to use *)
  | DFail
  | DSwitch of string * (string * decision_tree) list * decision_tree

(* {1 Matching} *)

let rec match_value p v =
  match p, v with
  | PWildcard, _ -> Some []  (* Wildcard matches anything, no bindings *)

  | PVar x, _ -> Some [(x, v)]  (* Variable binds the value *)

  | PConst v1, v2 ->
      if v1 = v2 then Some [] else None

  | PTuple ps, VTuple vs ->
      if List.length ps <> List.length vs then None
      else
        let results = List.map2 match_value ps vs in
        if List.for_all Option.is_some results then
          Some (List.flatten (List.map Option.get results))
        else None

  | PRecord flds, VRecord vflds ->
      let rec match_fields = function
        | [], _ -> Some []
        | (l, p) :: rest, vflds ->
            (try
               let v = List.assoc l vflds in
               (match match_value p v with
                | None -> None
                | Some bindings -> match_fields (rest, vflds)
                    |> Option.map (fun rest_bindings -> bindings @ rest_bindings))
             with Not_found -> None)
      in
      match_fields (flds, vflds)

  | PVariant (c1, None), VVariant (c2, None) ->
      if c1 = c2 then Some [] else None

  | PVariant (c1, Some p), VVariant (c2, Some v) ->
      if c1 = c2 then match_value p v else None

  | POr (p1, p2), v ->
      (match match_value p1 v with
       | Some _ as result -> result
       | None -> match_value p2 v)

  | PAs (x, p), v ->
      (match match_value p v with
       | Some bindings -> Some ((x, v) :: bindings)
       | None -> None)

  | _, _ -> None

let matches p v =
  match_value p v <> None

(* {1 Decision Tree Compilation} *)

(* Compile a single pattern with a success continuation *)
let rec compile_pattern_aux p (succ_vars : string list) =
  match p with
  | PWildcard -> DLeaf succ_vars

  | PVar x -> DLeaf (x :: succ_vars)

  | PConst _v ->
      (* Const matching happens at match time *)
      DLeaf succ_vars  (* Simplified - real impl would have const test nodes *)

  | PTuple _ps ->
      (* Compile tuple patterns sequentially *)
      (* Simplified - just return the success vars *)
      DLeaf succ_vars

  | PVariant (_c, None) ->
      DLeaf succ_vars

  | PVariant (_c, Some _p) ->
      (* Need to test variant and then match payload *)
      DLeaf succ_vars  (* Simplified *)

  | POr (p1, p2) ->
      (* Compile both branches and try p1, fallback to p2 *)
      let t1 = compile_pattern_aux p1 succ_vars in
      let _t2 = compile_pattern_aux p2 succ_vars in
      t1  (* Simplified - would need OR node *)

  | PAs (x, p) ->
      let t = compile_pattern_aux p (x :: succ_vars) in
      t

  | PRecord _flds ->
      (* Simplified record pattern compilation *)
      DLeaf succ_vars

let compile_pattern p =
  compile_pattern_aux p []

let compile_patterns rules =
  (* Compile list of (pattern, expression) pairs *)
  match rules with
  | [] -> DFail
  | (p, _) :: _rest ->
      let t1 = compile_pattern p in
      (* Would compile rest for fallback *)
      t1

let eval_decision_tree _dt _env _v =
  (* Placeholder implementation *)
  None

(* {1 Exhaustiveness Checking} *)

module StringSet = Set.Make(String)

let is_exhaustive ps ty =
  match ty with
  | TBool ->
      (* Need to cover true and false *)
      let covers_true = List.exists (fun p ->
        match p with PConst (VBool true) -> true | _ -> false) ps
      and covers_false = List.exists (fun p ->
        match p with PConst (VBool false) -> true | _ -> false) ps in
      let covers_any = List.exists (fun p ->
        match p with PWildcard | PVar _ -> true | _ -> false) ps in
      covers_any || (covers_true && covers_false)

  | TUnit ->
      (* Only need to cover () *)
      List.exists (fun p ->
        match p with PConst VUnit | PWildcard | PVar _ -> true | _ -> false) ps

  | TVariant cases ->
      (* Need to cover all constructors *)
      let covered_constructors = List.fold_left (fun covered p ->
        match p with
        | PVariant (c, _) -> StringSet.add c covered
        | PWildcard | PVar _ -> List.fold_left (fun acc (c, _) -> StringSet.add c acc) covered cases
        | _ -> covered
      ) StringSet.empty ps in
      let all_constructors = List.fold_left (fun acc (c, _) ->
        StringSet.add c acc) StringSet.empty cases in
      StringSet.subset all_constructors covered_constructors

  | _ -> true  (* Conservative for other types *)

let missing_patterns ps ty =
  if is_exhaustive ps ty then None
  else
    (* Compute missing patterns *)
    match ty with
    | TBool ->
        let has_true = List.exists (fun p ->
          match p with PConst (VBool true) -> true | _ -> false) ps in
        let has_false = List.exists (fun p ->
          match p with PConst (VBool false) -> true | _ -> false) ps in
        let missing =
          (if has_true then [] else [PConst (VBool true)]) @
          (if has_false then [] else [PConst (VBool false)])
        in
        if missing = [] then None else Some missing
    | _ -> Some [PWildcard]

(* {1 Pretty Printing} *)

let rec string_of_pattern = function
  | PWildcard -> "_"
  | PVar x -> x
  | PConst (VBool b) -> string_of_bool b
  | PConst (VInt n) -> string_of_int n
  | PConst (VString s) -> Printf.sprintf "\"%s\"" s
  | PConst VUnit -> "()"
  | PConst (VTuple _) -> "<tuple>"
  | PConst (VRecord _) -> "<record>"
  | PConst (VVariant _) -> "<variant>"
  | PConst (VClosure _) -> "<closure>"
  | PTuple ps ->
      let elems = List.map string_of_pattern ps in
      "(" ^ String.concat ", " elems ^ ")"
  | PRecord flds ->
      let fs = List.map (fun (l, p) -> l ^ "=" ^ string_of_pattern p) flds in
      "{" ^ String.concat "; " fs ^ "}"
  | PVariant (c, None) -> c
  | PVariant (c, Some p) -> Printf.sprintf "%s(%s)" c (string_of_pattern p)
  | POr (p1, p2) ->
      Printf.sprintf "(%s | %s)" (string_of_pattern p1) (string_of_pattern p2)
  | PAs (x, p) ->
      Printf.sprintf "%s as %s" (string_of_pattern p) x

let rec string_of_decision_tree = function
  | DLeaf vars -> "Success(" ^ String.concat ", " vars ^ ")"
  | DFail -> "Fail"
  | DSwitch (v, cases, def) ->
      let case_strs = List.map (fun (c, t) ->
        c ^ " => " ^ string_of_decision_tree t) cases in
      Printf.sprintf "Switch(%s) { %s | default: %s }"
        v (String.concat "; " case_strs) (string_of_decision_tree def)
