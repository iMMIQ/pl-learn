(** Control Operators: call/cc and exceptions *)

(** {1 Extended CPS with call/cc} *)

type cps_expr_ext =
  | EConst of int
  | EVar of string
  | EPrim of string * cps_expr_ext list * cps_cont_ext
  | ELet of string * cps_expr_ext * cps_cont_ext
  | ECallCC of string * cps_cont_ext  (* call/cc *)

and cps_cont_ext =
  | EHalt
  | EArg of string * cps_cont_ext
  | EBin1 of string * cps_expr_ext * cps_cont_ext
  | EFunc of cps_expr_ext * cps_cont_ext

(** {1 Value Type} *)

type value =
  | VInt of int
  | VCont of cps_cont_ext  (* First-class continuation *)

(** {1 Evaluation with First-Class Continuations} *)

val eval_ext : (string -> value) -> cps_expr_ext -> value

(** {1 Example: Capturing and Invoking Continuations} *)

val example_callcc : cps_expr_ext
val example_exception : cps_expr_ext
