(** Lesson 14: Compiler Backend - Source Language AST *)

(** {1 Source Language} *)

(** A simple functional language with:
    - Integers and booleans
    - Variables and let bindings
    - Lambda abstractions and function application
    - Primitive operations
    - Conditionals
*)

(** {1 Expressions} *)

type binop =
  | Add | Sub | Mul | Div
  | Eq | Lt | Gt

type expr =
  | EInt of int                      (* Integer literal *)
  | EBool of bool                    (* Boolean literal *)
  | EVar of string                   (* Variable reference *)
  | EAbs of string list * expr       (* Lambda: λx1...xn. e *)
  | EApp of expr * expr list         (* Application: e e1 e2 ... en *)
  | ELet of string * expr * expr     (* Let binding *)
  | EPrim of binop * expr * expr     (* Primitive operation *)
  | EIf of expr * expr * expr        (* Conditional *)
  | ESeq of expr list                (* Sequence: e1; e2; ...; en *)

(** {1 Value Representation} *)

type value =
  | VInt of int
  | VBool of bool
  | VClosure of int * string list    (* (addr, params) *)
  | VUnit

(** {1 Pretty Printing} *)

val string_of_expr : expr -> string
val string_of_binop : binop -> string
val string_of_value : value -> string
