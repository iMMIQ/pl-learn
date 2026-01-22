type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

type value =
  | VInt of int

let is_value = function
  | Const _ -> true
  | _ -> false

(* {1 Single Step Reduction} *)

let rec step = function
  (* Add *)
  | Add (Const n1, Const n2) ->
      Some (Const (n1 + n2))
  | Add (Const _, e2) when is_value e2 ->
      None (* stuck - shouldn't happen with well-formed terms *)
  | Add (e1, e2) ->
      (match step e1 with
       | Some e1' -> Some (Add (e1', e2))
       | None when is_value e1 ->
           (match step e2 with
            | Some e2' -> Some (Add (e1, e2'))
            | None -> None)
       | None -> Some (Add (e1, e2)))

  (* Mul *)
  | Mul (Const n1, Const n2) ->
      Some (Const (n1 * n2))
  | Mul (e1, e2) ->
      (match step e1 with
       | Some e1' -> Some (Mul (e1', e2))
       | None when is_value e1 ->
           (match step e2 with
            | Some e2' -> Some (Mul (e1, e2'))
            | None -> None)
       | None -> Some (Mul (e1, e2)))

  (* Let *)
  | Let (x, Const v, e2) ->
      Some (subst x v e2)
  | Let (x, e1, e2) ->
      (match step e1 with
       | Some e1' -> Some (Let (x, e1', e2))
       | None -> None)

  (* Values - no reduction *)
  | Const _ -> None

  (* Variables - stuck unless substituted *)
  | Var _ -> None

and subst x v = function
  | Const n -> Const n
  | Add (e1, e2) -> Add (subst x v e1, subst x v e2)
  | Mul (e1, e2) -> Mul (subst x v e1, subst x v e2)
  | Var y -> if y = x then Const v else Var y
  | Let (y, e1, e2) ->
      Let (y, subst x v e1, if y = x then e2 else subst x v e2)

(* {1 Multi-Step Reduction} *)

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Add (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) ->
      "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Var x -> x
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let steps e =
  let rec loop e acc =
    match step e with
    | None -> List.rev (e :: acc)
    | Some e' -> loop e' (e :: acc)
  in
  loop e []

let reduce ?(max_steps=1000) e =
  let rec loop e count =
    if count >= max_steps then None
    else
      match step e with
      | None -> Some e
      | Some e' -> loop e' (count + 1)
  in
  loop e 0

let trace e =
  let seq = steps e in
  String.concat "\n  → " (List.map string_of_expr seq)
