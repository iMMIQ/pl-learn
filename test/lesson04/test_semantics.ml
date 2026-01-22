open Alcotest
module S = Lesson04.Smallstep
module B = Lesson04.Bigstep

(* Small-step tests *)

let test_step_add_consts _ctx =
  let e = S.Add (S.Const 3, S.Const 5) in
  let r = S.step e in
  match r with
  | Some (S.Const 8) -> ()
  | _ -> failwith "Expected 3+5 → 8"

let test_step_nested _ctx =
  let e = S.Add (S.Add (S.Const 1, S.Const 2), S.Const 3) in
  let r = S.step e in
  match r with
  | Some (S.Add (S.Const 3, S.Const 3)) -> ()
  | _ -> failwith "Expected (1+2)+3 → 3+3"

let test_steps_trace _ctx =
  let e = S.Add (S.Add (S.Const 1, S.Const 2), S.Const 3) in
  let seq = S.steps e in
  check int "trace length" 3 (List.length seq)

let test_reduce_normal_form _ctx =
  let e = S.Add (S.Mul (S.Const 2, S.Const 3), S.Const 4) in
  let r = S.reduce e in
  match r with
  | Some (S.Const 10) -> ()
  | _ -> failwith "Expected 2*3+4 = 10"

let test_step_let _ctx =
  let e = S.Let ("x", S.Const 5, S.Add (S.Var "x", S.Const 3)) in
  let r = S.step e in
  match r with
  | Some (S.Add (S.Const 5, S.Const 3)) -> ()
  | _ -> failwith "Expected let x=5 in x+3 → 5+3"

(* Big-step tests *)

let test_bigstep_const _ctx =
  let r = B.eval (fun _ -> raise Not_found) (B.Const 42) in
  check int "Const 42 ⇓ 42" 42 r

let test_bigstep_add _ctx =
  let e = B.Add (B.Const 3, B.Const 5) in
  let r = B.eval (fun _ -> raise Not_found) e in
  check int "3+5 ⇓ 8" 8 r

let test_bigstep_let _ctx =
  let e = B.Let ("x", B.Const 10, B.Add (B.Var "x", B.Const 5)) in
  let r = B.eval (fun _ -> raise Not_found) e in
  check int "let x=10 in x+5 ⇓ 15" 15 r

let test_bigstep_nested _ctx =
  let e = B.Let ("x", B.Const 5,
               B.Let ("y", B.Const 3,
                    B.Mul (B.Var "x", B.Var "y"))) in
  let r = B.eval (fun _ -> raise Not_found) e in
  check int "nested let" 15 r

let () =
  run "Lesson04: Operational Semantics" [
    ("Small-Step", [
      test_case "3+5 → 8" `Quick test_step_add_consts;
      test_case "(1+2)+3 → 3+3" `Quick test_step_nested;
      test_case "trace length" `Quick test_steps_trace;
      test_case "2*3+4 = 10" `Quick test_reduce_normal_form;
      test_case "let reduction" `Quick test_step_let;
    ]);
    ("Big-Step", [
      test_case "Const 42" `Quick test_bigstep_const;
      test_case "3+5 ⇓ 8" `Quick test_bigstep_add;
      test_case "let evaluation" `Quick test_bigstep_let;
      test_case "nested let" `Quick test_bigstep_nested;
    ]);
  ]
