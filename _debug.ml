open Lesson02.Lambda

let () =
  let e = church_zero in
  let test_expr = App (App (e, Abs ("n", Abs ("x", App (Var "x", Var "n")))), Abs ("_", Var "_")) in
  Printf.printf "test_expr = %s\n" (string_of_expr test_expr);
  match normalize ~max_steps:1000 test_expr with
  | None -> Printf.printf "Normalized: None\n"
  | Some e' ->
      Printf.printf "Normalized: %s\n" (string_of_expr e')
