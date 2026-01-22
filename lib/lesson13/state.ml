(* {1 State Type} *)

type ('s, 'a) t = 's -> 'a * 's

(* {1 Monad Operations} *)

let return x s = (x, s)

let bind m f s =
  let (x, s') = m s in
  f x s'

let map f m s =
  let (x, s') = m s in
  (f x, s')

let join mm s =
  let (m, s') = mm s in
  m s'

(* {1 State Operations} *)

let get s = (s, s)

let put s _ = ((), s)

let modify f s = ((), f s)

let gets f s = (f s, s)

(* {1 Running Computations} *)

let run_state init m = m init

let eval_state init m =
  let (result, _) = m init in
  result

let exec_state init m =
  let (_, state) = m init in
  state

(* {1 Common Patterns} *)

let counter () = bind get (fun n ->
  bind (put (n + 1)) (fun () ->
  return n))

let sum nums =
  bind get (fun acc ->
  let new_acc = List.fold_left (+) acc nums in
  bind (put new_acc) (fun () ->
  return ()))

let collect items =
  bind get (fun lst ->
  bind (put (List.rev_append items lst)) (fun () ->
  return ()))

(* {1 Stack Operations} *)

type 'a stack = 'a list

let push x = bind get (fun stack ->
  bind (put (x :: stack)) (fun () ->
  return ()))

let pop () = bind get (fun stack ->
  match stack with
  | [] -> return None
  | x :: xs ->
      bind (put xs) (fun () ->
      return (Some x)))

let peek () = bind get (fun stack ->
  match stack with
  | [] -> return None
  | x :: _ -> return (Some x))

(* {1 Tagged Values (Fresh Name Generation) *)

let counter_ref = ref 0

let fresh () =
  let n = !counter_ref in
  incr counter_ref;
  return ("v" ^ string_of_int n)

(* Alternative pure version using state threading *)

let fresh_pure () = bind get (fun n ->
  bind (put (n + 1)) (fun () ->
  return ("v" ^ string_of_int n)))
