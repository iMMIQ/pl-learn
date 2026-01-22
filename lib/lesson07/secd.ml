(* {1 Abstract Syntax} *)

type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr
  | Const of int
  | Add of expr * expr
  | Let of string * expr * expr

(* {1 Instructions} *)

type instr =
  | IConst of int
  | IAccess of int
  | IClose of instr list
  | IApp
  | IReturn
  | IAdd
  | ILet of instr list * instr list

(* {1 Values} *)

type value =
  | VInt of int
  | VClosure of string * instr list * value array

(* {1 Machine State} *)

type secd = {
  stack : value list;
  env : value array list;
  control : instr list;
  dump : secd list;
}

(* {1 Pretty Printing} *)

let string_of_value = function
  | VInt n -> string_of_int n
  | VClosure (x, _, _) -> Printf.sprintf "<λ%s. ...>" x

let string_of_instr = function
  | IConst n -> "Const(" ^ string_of_int n ^ ")"
  | IAccess i -> "Access(" ^ string_of_int i ^ ")"
  | IClose _ -> "Close(...)"
  | IApp -> "App"
  | IReturn -> "Return"
  | IAdd -> "Add"
  | ILet _ -> "Let(...)"

let _string_of_instrs instrs =
  String.concat "; " (List.map string_of_instr instrs)

let string_of_state s =
  Printf.sprintf
    {|S: [%s]
E: %d frame(s)
C: [%s]
D: %d saved|}
    (String.concat ", " (List.map string_of_value s.stack))
    (List.length s.env)
    (match s.control with
     | [] -> "empty"
     | _ -> String.concat "; " (List.map string_of_instr (List.take 3 s.control)) ^ "...")
    (List.length s.dump)

(* {1 Compilation} *)

(* Variable lookup during compilation *)
(* Returns (depth, index) into environment chain *)
let lookup_var x env =
  let rec lookup depth idx = function
    | [] -> raise (Not_found)
    | frame :: rest ->
        if List.mem x frame then (depth, idx)
        else
          let idx' = idx + List.length frame in
          lookup depth idx' rest
  in
  lookup 0 0 env

(* Compile expression with variable environment *)
let rec compile env = function
  | Const n ->
      [IConst n]

  | Add (e1, e2) ->
      compile env e1 @
      compile env e2 @
      [IAdd]

  | Var x ->
      let (depth, _idx) = lookup_var x env in
      (* Calculate access index: depth * frame_size + idx *)
      (* Simplified: just use depth for now *)
      [IAccess depth]

  | Abs (x, e) ->
      let body_code = compile ([x] :: env) e in
      [IClose body_code]

  | App (e1, e2) ->
      compile env e1 @
      compile env e2 @
      [IApp]

  | Let (x, e1, e2) ->
      let body1 = compile env e1 in
      let body2 = compile ([x] :: env) e2 in
      [ILet (body1, body2)]

let compile e = compile [] e

(* {1 Machine Execution} *)

let init e =
  {
    stack = [];
    env = [];
    control = compile e;
    dump = [];
  }

let rec _get_var depth idx env =
  match depth, env with
  | 0, frame :: _ ->
      (try frame.(idx)
       with Invalid_argument _ -> raise (Failure "Unbound"))
  | n, _ :: rest -> _get_var (n - 1) idx rest
  | _ -> raise (Failure "Unbound")

let step (s : secd) : secd =
  match s.control with
  | [] ->
      (* Halt *)
      { s with control = [] }

  | i :: rest ->
      match i with
      | IConst n ->
          { s with
            stack = VInt n :: s.stack;
            control = rest;
          }

      | IAccess depth ->
          (* Simplified: access from environment *)
          (match s.env with
           | frame :: _ ->
               (try
                  let v = frame.(depth) in
                  { s with
                    stack = v :: s.stack;
                    control = rest;
                  }
                with Invalid_argument _ ->
                    raise (Failure "Unbound variable"))
           | [] -> raise (Failure "Empty environment"))

      | IClose body_code ->
          (match s.stack with
           | [] -> raise (Failure "Close: empty stack")
           | _ :: _ ->
               (* Closure captures current environment *)
               (* For simplicity, closures don't capture yet *)
               let closure = VClosure ("x", body_code, [||]) in
               { s with
                 stack = closure :: s.stack;
                 control = rest;
               })

      | IApp ->
          (match s.stack with
           | VClosure (_x, body_code, _) :: arg :: rest_stack ->
               (* Create new environment frame *)
               let new_env = [| arg |] in
               (* Save current state *)
               let saved = {
                 stack = rest_stack;
                 env = s.env;
                 control = rest;
                 dump = s.dump;
               } in
               {
                 stack = [];
                 env = new_env :: s.env;
                 control = body_code @ [IReturn];
                 dump = saved :: s.dump;
               }
           | _ -> raise (Failure "App: type error"))

      | IReturn ->
          (match s.dump with
           | saved :: rest_dump ->
               (* Return value is on top of stack *)
               let ret_val = match s.stack with
                 | v :: _ -> v
                 | [] -> VInt 0
               in
               { saved with
                 stack = ret_val :: saved.stack;
                 dump = rest_dump;
               }
           | [] -> { s with control = [] })

      | IAdd ->
          (match s.stack with
           | VInt n2 :: VInt n1 :: rest_stack ->
               { s with
                 stack = VInt (n1 + n2) :: rest_stack;
                 control = rest;
               }
           | _ -> raise (Failure "Add: type error"))

      | ILet (body1, body2) ->
          (* Execute body1, then extend env with result, then body2 *)
          { s with
            control = body1 @ [IClose body2] @ rest;
          }

let rec run s =
  match s.control with
  | [] ->
      (match s.stack with
       | v :: _ -> v
       | [] -> VInt 0)
  | _ ->
      run (step s)
