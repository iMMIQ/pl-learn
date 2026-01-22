(* {1 Stream Type} *)

type 'a stream =
  | Nil
  | Cons of 'a * 'a stream lazy_cell

and 'a lazy_cell = {
  mutable state : 'a lazy_state;
}

and 'a lazy_state =
  | LUnevaluated of (unit -> 'a)
  | LEvaluating
  | LEvaluated of 'a

(* {1 Lazy Cell Operations} *)

let make_lazy f = { state = LUnevaluated f }

let force t =
  match t.state with
  | LUnevaluated f ->
      t.state <- LEvaluating;
      let result = f () in
      t.state <- LEvaluated result;
      result
  | LEvaluating -> failwith "Cyclic stream"
  | LEvaluated v -> v

(* {1 Stream Construction} *)

let cons x xs = Cons (x, make_lazy (fun () -> xs))

let nil = Nil

(* {1 Stream Operations} *)

let rec take n s =
  match n, s with
  | 0, _ -> []
  | _, Nil -> []
  | n, Cons (x, xs) ->
      x :: take (n - 1) (force xs)

let rec drop n s =
  match n, s with
  | 0, _ -> s
  | _, Nil -> Nil
  | n, Cons (_, xs) -> drop (n - 1) (force xs)

let rec map f = function
  | Nil -> Nil
  | Cons (x, xs) ->
      Cons (f x, make_lazy (fun () -> map f (force xs)))

let rec filter p = function
  | Nil -> Nil
  | Cons (x, xs) ->
      if p x then
        Cons (x, make_lazy (fun () -> filter p (force xs)))
      else
        filter p (force xs)

let rec fold_left f init s limit =
  match s with
  | Nil -> init
  | Cons (x, xs) ->
      if limit <= 0 then init
      else fold_left f (f init x) (force xs) (limit - 1)

(* {1 Infinite Stream Generators} *)

let rec nats n =
  Cons (n, make_lazy (fun () -> nats (n + 1)))

let from_step start step =
  let rec loop n =
    Cons (n, make_lazy (fun () -> loop (n + step)))
  in
  loop start

let rec fibs_aux a b =
  Cons (a, make_lazy (fun () -> fibs_aux b (a + b)))

let fibs = fibs_aux 0 1

(* Sieve of Eratosthenes for primes *)
let rec sieve = function
  | Nil -> Nil
  | Cons (p, xs) ->
      let filtered = filter (fun n -> n mod p <> 0) (force xs) in
      Cons (p, make_lazy (fun () -> sieve filtered))

let primes = sieve (Cons (2, make_lazy (fun () -> nats 3)))

(* {1 Stream Zipping} *)

let rec zip s1 s2 =
  match s1, s2 with
  | Nil, _ | _, Nil -> Nil
  | Cons (x1, xs1), Cons (x2, xs2) ->
      Cons ((x1, x2),
             make_lazy (fun () ->
               zip (force xs1) (force xs2)))

let rec zip_with f s1 s2 =
  match s1, s2 with
  | Nil, _ | _, Nil -> Nil
  | Cons (x1, xs1), Cons (x2, xs2) ->
      Cons (f x1 x2,
             make_lazy (fun () ->
               zip_with f (force xs1) (force xs2)))

(* {1 Pretty Printing} *)

let string_of_stream n to_string s =
  let elems = take n s |> List.map to_string in
  "[" ^ String.concat "; " elems ^ (if n > 0 then "; ..." else "") ^ "]"
