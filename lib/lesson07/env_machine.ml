open Secd

(* {1 Instructions} *)

type instr =
  | IConst of int
  | IVar of string
  | IAbs of string * instr list
  | IApp
  | IAdd
  | ILet of string * instr list * instr list

(* {1 Closure} *)

type closure = {
  param : string;
  body : instr list;
  env : (string * value) list;
}

and value =
  | VInt of int
  | VClos of closure

(* {1 Machine State} *)

type state = {
  code : instr list;
  env : (string * value) list;
  stack : value list;
}

(* {1 Compilation} *)

let rec compile = function
  | Const n -> [IConst n]
  | Var x -> [IVar x]
  | Add (e1, e2) -> compile e1 @ compile e2 @ [IAdd]
  | Abs (x, e) -> [IAbs (x, compile e)]
  | App (e1, e2) -> compile e1 @ compile e2 @ [IApp]
  | Let (x, e1, e2) ->
      (* let x = e1 in e2  desugars to  (λx. e2) e1 *)
      compile (App (Abs (x, e2), e1))

(* {1 Environment Operations} *)

let env_lookup x env =
  try List.assoc x env
  with Not_found -> raise (Failure ("Unbound: " ^ x))

let env_extend x v env =
  (x, v) :: env

(* {1 Execution} *)

let init_state e =
  { code = compile e;
    env = [];
    stack = [];
  }

let step_state s =
  match s.code with
  | [] -> s  (* Halted *)

  | i :: rest ->
      match i with
      | IConst n ->
          { s with
            code = rest;
            stack = VInt n :: s.stack;
          }

      | IVar x ->
          { s with
            code = rest;
            stack = env_lookup x s.env :: s.stack;
          }

      | IAbs (x, body) ->
          let clos = VClos { param = x; body = body; env = s.env; } in
          { s with
            code = rest;
            stack = clos :: s.stack;
          }

      | IApp ->
          (match s.stack with
           | arg :: VClos clos :: _rest_stack ->
               (* Execute function body with extended environment *)
               (* Start with empty stack - body will push result *)
               (* After body completes, result will be on stack *)
               (* Then continue with rest of instructions *)
               {
                 code = clos.body @ rest;
                 env = env_extend clos.param arg clos.env;
                 stack = [];
               }
           | _ -> raise (Failure "App: type error"))

      | IAdd ->
          (match s.stack with
           | VInt n2 :: VInt n1 :: rest_stack ->
               { s with
                 code = rest;
                 stack = VInt (n1 + n2) :: rest_stack;
               }
           | _ -> raise (Failure "Add: type error"))

      | ILet _ ->
          (* Should not happen - let is desugared during compilation *)
          raise (Failure "ILet: should be desugared")

let rec eval_state s =
  match s.code with
  | [] ->
      (match s.stack with
       | v :: _ -> v
       | [] -> VInt 0)
  | _ -> eval_state (step_state s)

let eval e = eval_state (init_state e)

(* {1 Tracing} *)

let string_of_value = function
  | VInt n -> string_of_int n
  | VClos _ -> "<closure>"

let rec run_steps s count max =
  if count >= max then ["(max steps reached)"]
  else if s.code = [] then ["halt"]
  else
    let step_str = Printf.sprintf "  S=[%s] C=%d"
      (String.concat ", " (List.map string_of_value (List.take 3 s.stack)))
      (List.length s.code) in
    step_str :: run_steps (step_state s) (count + 1) max

let trace e =
  let steps = run_steps (init_state e) 0 20 in
  String.concat "\n" steps
