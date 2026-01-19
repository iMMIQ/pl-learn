type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr

type strategy =
  | Normal_order
  | Applicative_order

(* {1 Free Variables} *)

let rec free_vars = function
  | Var x -> [x]
  | Abs (x, e) -> List.filter ((<>) x) (free_vars e)
  | App (e1, e2) ->
      free_vars e1 @ free_vars e2
      |> List.sort_uniq String.compare

let is_closed e = free_vars e = []

(* {1 Substitution with Capture Avoidance} *)

let fresh x used =
  let rec gen suffix =
    let candidate = x ^ "_" ^ string_of_int suffix in
    if List.mem candidate used then gen (succ suffix)
    else candidate
  in
  if List.mem x used then gen 1 else x

let rec subst x e = function
  | Var y as expr ->
      if y = x then e else expr
  | Abs (y, body) as expr ->
      if y = x then expr
      else if List.mem y (free_vars e) then
        let used = x :: free_vars body @ free_vars e in
        let y' = fresh y used in
        let body' = subst y (Var y') body in
        Abs (y', subst x e body')
      else
        Abs (y, subst x e body)
  | App (e1, e2) ->
      App (subst x e e1, subst x e e2)

(* {1 Beta Reduction} *)

let rec reduce_one strategy = function
  | Var _ | Abs _ -> None
  | App (Abs (x, body), arg) ->
      Some (subst x arg body)
  | App (e1, e2) ->
      (match reduce_one strategy e1 with
       | Some e1' -> Some (App (e1', e2))
       | None ->
           match reduce_one strategy e2 with
           | Some e2' -> Some (App (e1, e2'))
           | None -> None)

let normalize ?(strategy=Normal_order) ?(max_steps=10000) e =
  let rec step e count =
    if count >= max_steps then None
    else
      match reduce_one strategy e with
      | None -> Some e
      | Some e' -> step e' (count + 1)
  in
  step e 0

(* {1 Church Encodings} *)

let church_true = Abs ("p", Abs ("q", Var "p"))
let church_false = Abs ("p", Abs ("q", Var "q"))
let church_if = Abs ("b", Abs ("t", Abs ("f",
  App (App (Var "b", Var "t"), Var "f"))))

let church_pair =
  Abs ("f", Abs ("s", Abs ("b",
    App (App (Var "b", Var "f"), Var "s"))))
let church_fst =
  Abs ("p", App (Var "p", Abs ("x", Abs ("y", Var "x"))))
let church_snd =
  Abs ("p", App (Var "p", Abs ("x", Abs ("y", Var "y"))))

let church_zero = Abs ("s", Abs ("z", Var "z"))

let church_succ =
  Abs ("n", Abs ("s", Abs ("z",
    App (Var "s", App (App (Var "n", Var "s"), Var "z")))))

let church_add =
  Abs ("m", Abs ("n", Abs ("s", Abs ("z",
    App (App (Var "m", Var "s"), App (App (Var "n", Var "s"), Var "z"))))))

let church_mul =
  Abs ("m", Abs ("n", Abs ("f",
    App (Var "m", App (Var "n", Var "f")))))

let rec int_to_church n =
  if n <= 0 then church_zero
  else App (church_succ, int_to_church (n - 1))

let church_to_int e =
  match normalize ~max_steps:1000
        (App (App (e, Abs ("n", Abs ("x", App (Var "x", Var "n")))),
              Abs ("_", Var "_"))) with
  | None -> None
  | Some e' ->
      let rec count = function
        | App (Abs ("_", Var "_"), _) -> 0
        | App (Abs (_, App (Var "x", Var "n")), rest) -> 1 + count rest
        | _ -> -1
      in
      try Some (count e') with _ -> None

(* {1 Pretty Printing} *)

let rec string_of_expr = function
  | Var x -> x
  | Abs (x, e) -> "λ" ^ x ^ ". " ^ string_of_expr e
  | App (e1, e2) ->
      let left = match e1 with
        | Abs _ -> "(" ^ string_of_expr e1 ^ ")"
        | App _ -> "(" ^ string_of_expr e1 ^ ")"
        | _ -> string_of_expr e1
      in
      let right = match e2 with
        | Var _ | Abs _ -> string_of_expr e2
        | _ -> "(" ^ string_of_expr e2 ^ ")"
      in
      left ^ " " ^ right

let pp_expr fmt e =
  Format.pp_print_string fmt (string_of_expr e)
