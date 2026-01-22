(** Compiler: AST to Bytecode *)

(** {1 Compilation Context} *)

(** Tracks variable bindings and label generation *)
type ctx = {
  mutable env : (string * int) list;  (* (var, depth) pairs *)
  mutable counter : int;               (* For generating fresh labels *)
  mutable closures : (string * string list * Ast.expr) list;
                                          (* Pending closures to compile *)
}

(** {1 Compilation} *)

(** [compile_expr ctx e] compiles expression [e] to bytecode.
    Returns (program, ctx) where ctx may have new closures to compile.
*)
val compile_expr : ctx -> Ast.expr -> Bytecode.program * ctx

(** [compile e] is the main entry point. Compiles a complete program. *)
val compile : Ast.expr -> Bytecode.program

(** {1 Closure Compilation} *)

(** [compile_all_closures ctx] compiles all pending closures into the program. *)
val compile_all_closures : ctx -> Bytecode.program -> Bytecode.program

(** {1 Label Generation} *)

(** [fresh_label ctx prefix] generates a fresh label with given prefix. *)
val fresh_label : ctx -> string -> string * ctx
