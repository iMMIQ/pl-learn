open Lesson12__Functor_examples

let () =
  Printf.printf "\n=== Lesson 12: Modules and Functors ===\n\n";

  Printf.printf "1. Module Signatures (Interfaces):\n";
  Printf.printf "   module type Comparable = sig\n";
  Printf.printf "     type t\n";
  Printf.printf "     val compare : t -> t -> int\n";
  Printf.printf "   end\n\n";

  Printf.printf "2. Module Structures (Implementations):\n";
  Printf.printf "   module IntComparable : Comparable = struct\n";
  Printf.printf "     type t = int\n";
  Printf.printf "     let compare = Stdlib.compare\n";
  Printf.printf "   end\n\n";

  Printf.printf "3. Functors (Functions from Modules):\n";
  Printf.printf "   module MakeBST (E : Comparable) = struct\n";
  Printf.printf "     type elt = E.t\n";
  Printf.printf "     type t = ...\n";
  Printf.printf "     val add : elt -> t -> t\n";
  Printf.printf "   end\n\n";

  Printf.printf "4. Functor Application:\n";
  Printf.printf "   module IntSet = MakeBST (IntComparable)\n";
  Printf.printf "   module StringSet = MakeBST (StringComparable)\n\n";

  Printf.printf "5. Using IntSet:\n";
  let s = IntSet.add 3 (IntSet.add 1 (IntSet.add 5 IntSet.empty)) in
  Printf.printf "   Set: [1; 3; 5] = %s\n" (String.concat "; "
    (List.map string_of_int (IntSet.to_list s)));
  Printf.printf "   mem 3: %b\n" (IntSet.mem 3 s);
  Printf.printf "   mem 7: %b\n\n" (IntSet.mem 7 s);

  Printf.printf "6. Using StringSet:\n";
  let ss = StringSet.add "cherry"
    (StringSet.add "banana"
      (StringSet.add "apple" StringSet.empty)) in
  Printf.printf "   Set: [apple; banana; cherry] = %s\n\n"
    (String.concat "; " (StringSet.to_list ss));

  Printf.printf "7. Monad Examples:\n";
  Printf.printf "   Option monad:\n";
  let result = OptionMonad.bind (Some 5) (fun x -> Some (x * 2)) in
  Printf.printf "     bind (Some 5) (*2) = %s\n"
    (match result with Some n -> "Some " ^ string_of_int n | None -> "None");

  let result = OptionMonad.bind None (fun x -> Some (x * 2)) in
  Printf.printf "     bind None (*2) = %s\n\n"
    (match result with Some n -> "Some " ^ string_of_int n | None -> "None");

  Printf.printf "   List monad:\n";
  let result = ListMonad.bind [1; 2; 3] (fun x -> [x; x * 10]) in
  Printf.printf "     bind [1;2;3] (fun x -> [x; x*10]) = %s\n\n"
    (String.concat "; " (List.map string_of_int result));

  Printf.printf "8. Type Abstraction:\n";
  Printf.printf "   module type STACK = sig\n";
  Printf.printf "     type t  (* Abstract! *)\n";
  Printf.printf "     val empty : t\n";
  Printf.printf "     val push : int -> t -> t\n";
  Printf.printf "     val pop : t -> (int * t) option\n";
  Printf.printf "   end\n\n";
  Printf.printf "   Clients cannot inspect the internal representation\n";
  Printf.printf "   They can only use the provided operations\n\n";

  Printf.printf "9. Module System Benefits:\n";
  Printf.printf "   ✓ Namespace management (avoid naming conflicts)\n";
  Printf.printf "   ✓ Information hiding (abstract types)\n";
  Printf.printf "   ✓ Code reuse (functors)\n";
  Printf.printf "   ✓ Separate compilation\n";
  Printf.printf "   ✓ Clear interfaces\n\n";

  Printf.printf "10. Real-World Module Systems:\n";
  Printf.printf "    - OCaml/Standard ML: Full module system with functors\n";
  Printf.printf "    - Haskell: Type classes (similar but different)\n";
  Printf.printf "    - Rust: Traits (interface-like, no functors)\n";
  Printf.printf "    - Java/C#: Interfaces and generics\n";
  Printf.printf "    - Scala: Traits + implicits (closest to ML modules)\n\n";

  Printf.printf "=== End of Examples ===\n"
