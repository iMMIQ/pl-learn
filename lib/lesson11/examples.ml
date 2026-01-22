(* {1 Option Type} *)

type 'a option =
  | Some of 'a
  | None

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let bind_option f = function
  | Some x -> f x
  | None -> None

(* {1 Result Type} *)

type ('e, 'a) result =
  | Ok of 'a
  | Error of 'e

let map_result f = function
  | Ok x -> Ok (f x)
  | Error e -> Error e

let bind_result f = function
  | Ok x -> f x
  | Error e -> Error e

(* {1 List Type} *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec list_map f = function
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, list_map f xs)

let rec list_fold_left f acc = function
  | Nil -> acc
  | Cons (x, xs) -> list_fold_left f (f acc x) xs

let list_length lst =
  list_fold_left (fun n _ -> n + 1) 0 lst

let rec list_reverse_aux acc = function
  | Nil -> acc
  | Cons (x, xs) -> list_reverse_aux (Cons (x, acc)) xs

let list_reverse lst =
  list_reverse_aux Nil lst

(* {1 Tree Type} *)

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec tree_map f = function
  | Leaf -> Leaf
  | Node (v, left, right) ->
      Node (f v, tree_map f left, tree_map f right)

let rec tree_fold f acc = function
  | Leaf -> acc
  | Node (v, left, right) ->
      let left_acc = tree_fold f acc left in
      let mid_acc = f v left_acc in
      tree_fold f mid_acc right

let tree_height t =
  tree_fold (fun _ left_h ->
    let right_h = left_h + 1 in
    max left_h right_h) 0 t

let tree_size t =
  tree_fold (fun _ acc -> acc + 1) 0 t

(* {1 Expression Tree} *)

type expr =
  | Lit of int
  | Add of expr * expr
  | Mul of expr * expr

let rec eval_expr = function
  | Lit n -> n
  | Add (e1, e2) -> eval_expr e1 + eval_expr e2
  | Mul (e1, e2) -> eval_expr e1 * eval_expr e2

let rec optimize_expr = function
  | Lit n -> Lit n
  | Add (e1, e2) ->
      (match optimize_expr e1, optimize_expr e2 with
       | Lit 0, e2' -> e2'  (* 0 + x = x *)
       | e1', Lit 0 -> e1'  (* x + 0 = x *)
       | Lit n1, Lit n2 -> Lit (n1 + n2)
       | e1', e2' -> Add (e1', e2'))
  | Mul (e1, e2) ->
      (match optimize_expr e1, optimize_expr e2 with
       | Lit 0, _ -> Lit 0  (* 0 * x = 0 *)
       | _, Lit 0 -> Lit 0  (* x * 0 = 0 *)
       | Lit 1, e2' -> e2'  (* 1 * x = x *)
       | e1', Lit 1 -> e1'  (* x * 1 = x *)
       | Lit n1, Lit n2 -> Lit (n1 * n2)
       | e1', e2' -> Mul (e1', e2'))
