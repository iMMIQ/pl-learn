(** Virtual Machine Interpreter *)

(** {1 VM State} *)

type vm = {
  mutable code : Bytecode.resolved_instr array;  (* Code memory *)
  mutable pc : int;                      (* Program counter *)
  mutable stack : Ast.value array;       (* Value stack *)
  mutable sp : int;                      (* Stack pointer *)
  mutable closures : Ast.value array array;  (* Closure environments *)
  mutable tracing : bool;                (* Enable trace output *)
}

(** {1 VM Operations} *)

(** [create_vm code] creates a new VM with given bytecode. *)
val create_vm : Bytecode.resolved_instr array -> vm

(** [reset vm] resets the VM to initial state. *)
val reset : vm -> unit

(** [push vm v] pushes value onto the stack. *)
val push : vm -> Ast.value -> unit

(** [pop vm] pops a value from the stack. *)
val pop : vm -> Ast.value

(** [peek vm n] peeks at value n positions from top of stack. *)
val peek : vm -> int -> Ast.value

(** {1 VM Execution} *)

(** [step vm] executes one instruction. Returns false if halted. *)
val step : vm -> bool

(** [run vm] executes until halt. Returns final stack top value. *)
val run : vm -> Ast.value

(** [run_from_entry vm entry] starts execution from label/addr. *)
val run_from_entry : vm -> string -> Ast.value

(** {1 Debugging} *)

(** [trace vm] executes with tracing enabled. *)
val trace : vm -> unit

(** [dump_state vm] prints current VM state. *)
val dump_state : vm -> unit

(** [disassemble vm] prints the bytecode. *)
val disassemble : vm -> string
