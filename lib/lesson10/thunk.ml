(* {1 Thunk Type} *)

type 'a thunk =
  | Unevaluated of (unit -> 'a)
  | Evaluating
  | Evaluated of 'a

(* {1 Thunk Operations} *)

let delay f = Unevaluated f

let force = function
  | Unevaluated f ->
      (* Mark as evaluating (cycle detection could go here) *)
      (* For simplicity, we don't track cycles in basic version *)
      let result = f () in
      result
  | Evaluating ->
      failwith "Cyclic thunk detected"
  | Evaluated v -> v

(* Note: The basic version above doesn't memoize.
   We need a mutable version for call-by-need. *)

(* {1 Mutable Thunk for Call-by-Need} *)

type 'a lazy_value = {
  mutable state : 'a thunk_state;
}

and 'a thunk_state =
  | LUnevaluated of (unit -> 'a)
  | LEvaluating
  | LEvaluated of 'a

let make_lazy f = { state = LUnevaluated f }

let get t =
  match t.state with
  | LUnevaluated f ->
      (* Mark as evaluating *)
      t.state <- LEvaluating;
      (* Compute the value *)
      let result = f () in
      (* Memoize the result *)
      t.state <- LEvaluated result;
      result
  | LEvaluating ->
      failwith "Cyclic lazy value detected"
  | LEvaluated v -> v

let status t =
  match t.state with
  | LUnevaluated _ -> `Unevaluated
  | LEvaluating -> `Evaluating
  | LEvaluated _ -> `Evaluated

let is_evaluated t =
  match t with
  | Unevaluated _ -> false
  | Evaluating -> false
  | Evaluated _ -> true

(* For testing only *)
let reset _ = ()

let string_of_status t =
  match status t with
  | `Unevaluated -> "Unevaluated"
  | `Evaluating -> "Evaluating"
  | `Evaluated -> "Evaluated"
