(** Lazy Lambda Calculus Evaluator *)

(** {1 Expression Syntax} *)

type expr =
  | Var of string                    (* Variable *)
  | Abs of string * expr             (* λx. e *)
  | App of expr * expr               (* e1 e2 *)
  | Const of int                     (* Constant *)
  | Prim of string * expr * expr     (* Primitive operation *)
  | If of expr * expr * expr         (* Conditional *)
  | Let of string * expr * expr      (* let x = e1 in e2 *)

(** {1 Values and Environment} *)

(** The type of runtime values *)
type value =
  | VConst of int
  | VClosure of string * expr * env  (* Closure with environment *)
  | VThunk of expr * env              (* Unevaluated expression *)

(** The type of environments (mapping from names to values) *)
and env = (string -> value option)

(** {1 Lazy Evaluation} *)

(** [eval env e] evaluates expression [e] in environment [env]
    using call-by-name (arguments not evaluated). *)
val eval : env -> expr -> value

(** [force_val v] forces evaluation of a thunk value. *)
val force_val : value -> value

(** [empty_env] is the empty environment. *)
val empty_env : env

(** [extend env x v] extends environment with binding [x -> v]. *)
val extend : env -> string -> value -> env

(** {1 Pretty Printing} *)

val string_of_expr : expr -> string
val string_of_value : value -> string
