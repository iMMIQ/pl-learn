(* {1 Expression Type} *)

type binop =
  | Add | Sub | Mul | Div
  | Eq | Lt | Gt

type expr =
  | EInt of int
  | EBool of bool
  | EVar of string
  | EAbs of string list * expr
  | EApp of expr * expr list
  | ELet of string * expr * expr
  | EPrim of binop * expr * expr
  | EIf of expr * expr * expr
  | ESeq of expr list

(* {1 Value Type} *)

type value =
  | VInt of int
  | VBool of bool
  | VClosure of int * string list  (* Code address, parameters *)
  | VUnit

(* {1 Pretty Printing} *)

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "="
  | Lt -> "<"
  | Gt -> ">"

let rec string_of_expr (_prec : int) = function
  | EInt n -> string_of_int n
  | EBool b -> string_of_bool b
  | EVar x -> x
  | EAbs (xs, e) ->
      let params = String.concat " " xs in
      let body = string_of_expr 0 e in
      Printf.sprintf "(λ%s. %s)" params body
  | EApp (f, args) ->
      let f_str = string_of_expr 2 f in
      let args_str = String.concat " " (List.map (string_of_expr 2) args) in
      Printf.sprintf "%s %s" f_str args_str
  | ELet (x, e1, e2) ->
      let e1_str = string_of_expr 0 e1 in
      let e2_str = string_of_expr 0 e2 in
      Printf.sprintf "(let %s = %s in %s)" x e1_str e2_str
  | EPrim (op, e1, e2) ->
      let e1_str = string_of_expr 1 e1 in
      let e2_str = string_of_expr 1 e2 in
      let op_str = string_of_binop op in
      Printf.sprintf "(%s %s %s)" op_str e1_str e2_str
  | EIf (e1, e2, e3) ->
      Printf.sprintf "(if %s then %s else %s)"
        (string_of_expr 0 e1) (string_of_expr 0 e2) (string_of_expr 0 e3)
  | ESeq exprs ->
      Printf.sprintf "(seq %s)"
        (String.concat "; " (List.map (string_of_expr 0) exprs))

let string_of_expr = string_of_expr 0

let string_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure (addr, params) ->
      Printf.sprintf "<closure@%d %s>" addr (String.concat " " params)
  | VUnit -> "()"
