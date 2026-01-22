(* {1 Types} *)

type ty =
  | TyTop
  | TyBot
  | TyBool
  | TyInt
  | TyString
  | TyRecord of (string * ty) list
  | TyArrow of ty * ty

exception SubtypeError of string

(* {1 Subtype Relation} *)

let rec is_subtype t1 t2 =
  match t1, t2 with
  (* Every type is a subtype of itself *)
  | _, _ when t1 = t2 -> true

  (* Bot is subtype of everything *)
  | TyBot, _ -> true

  (* Everything is subtype of Top *)
  | _, TyTop -> true

  (* Base types only subtype to Top *)
  | (TyBool | TyInt | TyString), (TyBool | TyInt | TyString) -> false

  (* Record subtyping: width and depth *)
  (* Width: more fields ≤ fewer fields *)
  (* Depth: if both have field l, then τ1.l ≤ τ2.l *)
  | TyRecord fields1, TyRecord fields2 ->
      (* All fields in t2 must exist in t1 *)
      List.for_all (fun (l2, t2_field) ->
        try
          let t1_field = List.assoc l2 fields1 in
          is_subtype t1_field t2_field
        with Not_found -> false
      ) fields2

  (* Function subtyping: contravariant in domain, covariant in codomain *)
  (* τ1 → τ2 ≤ σ1 → σ2 iff σ1 ≤ τ1 and τ2 ≤ σ2 *)
  | TyArrow (d1, c1), TyArrow (d2, c2) ->
      is_subtype d2 d1 &&  (* Contravariant domain *)
      is_subtype c1 c2     (* Covariant codomain *)

  | _ -> false

(* {1 Join (Least Upper Bound) *)

(* Helper: get all labels from a list of record fields *)
let all_labels fields =
  List.map fst fields |> List.sort_uniq String.compare

let rec join t1 t2 =
  match t1, t2 with
  | _, _ when t1 = t2 -> t1

  (* Top is identity for join *)
  | TyTop, _ | _, TyTop -> TyTop

  (* Bot join anything = the anything *)
  | TyBot, t -> t
  | t, TyBot -> t

  (* Base types: join is Top if different *)
  | (TyBool | TyInt | TyString), (TyBool | TyInt | TyString) when t1 <> t2 -> TyTop

  (* Record join *)
  | TyRecord f1, TyRecord f2 ->
      let labels = all_labels f1 @ all_labels f2 |> List.sort_uniq String.compare in
      let join_fields = List.map (fun l ->
        let t1_opt = try Some (List.assoc l f1) with Not_found -> None in
        let t2_opt = try Some (List.assoc l f2) with Not_found -> None in
        match t1_opt, t2_opt with
        | Some t1', Some t2' -> (l, join t1' t2')
        | Some t', None | None, Some t' -> (l, t')  (* Missing = Bot, join = t' *)
        | None, None -> (l, TyBot)
      ) labels in
      TyRecord join_fields

  (* Function join *)
  | TyArrow (d1, c1), TyArrow (d2, c2) ->
      (* Domain is meet (contravariant), codomain is join (covariant) *)
      TyArrow (meet d1 d2, join c1 c2)

  | _ -> TyTop

and meet t1 t2 =
  match t1, t2 with
  | _, _ when t1 = t2 -> t1

  (* Bot is identity for meet *)
  | TyBot, _ | _, TyBot -> TyBot

  (* Top meet anything = the anything *)
  | TyTop, t -> t
  | t, TyTop -> t

  (* Base types: meet is Bot if different *)
  | (TyBool | TyInt | TyString), (TyBool | TyInt | TyString) when t1 <> t2 -> TyBot

  (* Record meet *)
  | TyRecord f1, TyRecord f2 ->
      (* Only common fields *)
      let labels1 = all_labels f1 in
      let labels2 = all_labels f2 in
      let common_labels = List.filter (fun l -> List.mem l labels2) labels1 in
      let meet_fields = List.map (fun l ->
        let t1' = List.assoc l f1 in
        let t2' = List.assoc l f2 in
        (l, meet t1' t2')
      ) common_labels in
      TyRecord meet_fields

  (* Function meet *)
  | TyArrow (d1, c1), TyArrow (d2, c2) ->
      (* Domain is join (contravariant), codomain is meet (covariant) *)
      TyArrow (join d1 d2, meet c1 c2)

  | _ -> TyBot

(* {1 Pretty Printing} *)

let rec string_of_ty = function
  | TyTop -> "Top"
  | TyBot -> "Bot"
  | TyBool -> "Bool"
  | TyInt -> "Int"
  | TyString -> "String"
  | TyRecord fields ->
      let fields_str = String.concat ", " (List.map (fun (l, t) ->
        l ^ ":" ^ string_of_ty t
      ) fields) in
      "{ " ^ fields_str ^ " }"
  | TyArrow (t1, t2) ->
      let left = match t1 with
        | TyArrow _ -> "(" ^ string_of_ty t1 ^ ")"
        | _ -> string_of_ty t1
      in
      left ^ " → " ^ string_of_ty t2
