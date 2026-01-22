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
  | Var _ -> None
  | Abs (x, body) ->
      (* Try to reduce inside the body *)
      (match reduce_one strategy body with
       | Some body' -> Some (Abs (x, body'))
       | None -> None)
  | App (Abs (x, body), arg) ->
      (* Found a beta-redex *)
      (match strategy with
       | Normal_order -> Some (subst x arg body)
       | Applicative_order ->
           (* For applicative order, first try to reduce arg *)
           match reduce_one strategy arg with
           | Some arg' -> Some (App (Abs (x, body), arg'))
           | None -> Some (subst x arg body))
  | App (e1, e2) ->
      match strategy with
      | Normal_order ->
          (* Reduce leftmost first *)
          (match reduce_one strategy e1 with
           | Some e1' -> Some (App (e1', e2))
           | None ->
               match reduce_one strategy e2 with
               | Some e2' -> Some (App (e1, e2'))
               | None -> None)
      | Applicative_order ->
          (* Reduce argument first (rightmost), then function *)
          (match reduce_one strategy e2 with
           | Some e2' -> Some (App (e1, e2'))
           | None ->
               match reduce_one strategy e1 with
               | Some e1' -> Some (App (e1', e2))
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

let int_to_church n =
  (* Build Church numeral directly in normal form.
     Use unique bound variable names to avoid capture with church_add/mul.
     Church numeral n = λf. λx. f (f (... (f x)...)) with n applications of f *)
  let rec build_f_apps n x =
    if n <= 0 then x
    else App (Var "f", build_f_apps (n - 1) x)
  in
  Abs ("f", Abs ("x", build_f_apps n (Var "x")))

let church_to_int e =
  (* First normalize the expression, then count the function applications *)
  match normalize ~max_steps:1000 e with
  | None -> None
  | Some e' ->
      (* Count the number of function applications in Church numeral form
         Church n = λf. λx. f (f (... (f x)...)) with n applications of f *)
      let rec count = function
        | Abs (_, Abs (_, Var _)) -> 0
        | Abs (_, Abs (_, body)) ->
            (* body is of form f (f (... (f x)...)) *)
            let rec count_apps = function
              | Var "x" | Var "z" -> 0
              | App (Var v, inner) when v = "f" || v = "s" -> 1 + count_apps inner
              | App (e1, e2) -> max (count_apps e1) (count_apps e2)
              | _ -> 0
            in
            count_apps body
        | Abs (_, body) -> count body
        | _ -> 0
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
