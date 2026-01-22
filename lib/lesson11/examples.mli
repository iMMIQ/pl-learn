(** Classic ADT Examples *)

(** {1 Option Type} *)

type 'a option =
  | Some of 'a
  | None

(** {1 Result Type} *)

type ('e, 'a) result =
  | Ok of 'a
  | Error of 'e

(** {1 List Type} *)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

(** {1 Tree Type} *)

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(** {1 Expression Tree} *)

type expr =
  | Lit of int
  | Add of expr * expr
  | Mul of expr * expr

(** {1 Operations} *)

val map_option : ('a -> 'b) -> 'a option -> 'b option
val bind_option : ('a -> 'b option) -> 'a option -> 'b option

val map_result : ('a -> 'b) -> ('e, 'a) result -> ('e, 'b) result
val bind_result : ('a -> ('e, 'b) result) -> ('e, 'a) result -> ('e, 'b) result

val list_map : ('a -> 'b) -> 'a list -> 'b list
val list_fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val list_length : 'a list -> int
val list_reverse : 'a list -> 'a list

val tree_map : ('a -> 'b) -> 'a tree -> 'b tree
val tree_fold : ('a -> 'b -> 'b) -> 'b -> 'a tree -> 'b
val tree_height : 'a tree -> int
val tree_size : 'a tree -> int

val eval_expr : expr -> int
val optimize_expr : expr -> expr
