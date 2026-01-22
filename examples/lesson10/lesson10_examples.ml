open Lesson10.Thunk
open Lesson10.Stream

let () =
  Printf.printf "\n=== Lesson 10: Lazy Evaluation ===\n\n";

  Printf.printf "1. Strict vs Lazy Evaluation:\n";
  Printf.printf "   Strict: (λx. 1) (divergent) → ERROR\n";
  Printf.printf "   Lazy:  (λx. 1) (divergent) → 1 (argument never evaluated)\n\n";

  Printf.printf "2. Thunk Memoization:\n";
  let counter = ref 0 in
  let lv = make_lazy (fun () -> incr counter; !counter) in
  Printf.printf "   First get: %d (counter: %d)\n" (get lv) !counter;
  Printf.printf "   Second get: %d (counter: %d)\n" (get lv) !counter;
  Printf.printf "   Status: %s\n\n" (string_of_status lv);

  Printf.printf "3. Infinite Streams - Naturals:\n";
  let nats_stream = nats 0 in
  Printf.printf "   First 10: %s\n\n"
    (string_of_stream 10 string_of_int nats_stream);

  Printf.printf "4. Fibonacci Sequence:\n";
  Printf.printf "   First 15: %s\n\n"
    (string_of_stream 15 string_of_int fibs);

  Printf.printf "5. Prime Numbers (Sieve of Eratosthenes):\n";
  Printf.printf "   First 25 primes: %s\n\n"
    (string_of_stream 25 string_of_int primes);

  Printf.printf "6. Stream Operations:\n";
  let squares = map (fun n -> n * n) (nats 1) in
  Printf.printf "   Squares: %s\n" (string_of_stream 10 string_of_int squares);

  let evens = filter (fun n -> n mod 2 = 0) (nats 0) in
  Printf.printf "   Evens: %s\n\n" (string_of_stream 10 string_of_int evens);

  Printf.printf "7. Stream Zipping:\n";
  let sum_squares =
    zip_with (fun a b -> a * a + b * b) (nats 1) (nats 1) in
  Printf.printf "   Sum of squares pairs: %s\n\n"
    (string_of_stream 10 string_of_int sum_squares);

  Printf.printf "8. Advantages of Lazy Evaluation:\n";
  Printf.printf "   ✓ Modular code with infinite data structures\n";
  Printf.printf "   ✓ Avoid unnecessary computation\n";
  Printf.printf "   ✓ Enable data-driven control flow\n";
  Printf.printf "   ✓ Work with infinite streams\n\n";

  Printf.printf "9. Disadvantages:\n";
  Printf.printf "   ✗ Harder to reason about performance\n";
  Printf.printf "   ✗ Space leaks from retained thunks\n";
  Printf.printf "   ✗ Difficult to debug (when is it evaluated?)\n\n";

  Printf.printf "10. Languages with Lazy Evaluation:\n";
  Printf.printf "    - Haskell (pure lazy)\n";
  Printf.printf "    - OCaml (lazy keyword, Lazy.t module)\n";
  Printf.printf "    - Scala (Call-by-name, LazyVal)\n";
  Printf.printf "    - Racket (lazy, force)\n\n";

  Printf.printf "=== End of Examples ===\n"
