open Alcotest

(* Signature tests *)

let test_empty_sig () =
  let sig_items = Lesson12__Module_system.empty_sig in
  check int "empty signature length" 0 (List.length sig_items)

let test_add_type () =
  let sig_items = Lesson12__Module_system.add_type "t" Lesson12__Module_system.empty_sig in
  check int "signature with type" 1 (List.length sig_items)

let test_add_val () =
  let sig_items = Lesson12__Module_system.add_val "x" Lesson12__Module_system.TyInt Lesson12__Module_system.empty_sig in
  check int "signature with val" 1 (List.length sig_items)

let test_extend_sig () =
  let sig1 = Lesson12__Module_system.add_type "t" Lesson12__Module_system.empty_sig in
  let sig2 = Lesson12__Module_system.add_val "x" Lesson12__Module_system.TyInt Lesson12__Module_system.empty_sig in
  let combined = Lesson12__Module_system.extend sig1 sig2 in
  check int "extended signature length" 2 (List.length combined)

(* Structure tests *)

let test_empty_struct () =
  let struct_items = Lesson12__Module_system.empty_struct in
  check int "empty structure length" 0 (List.length struct_items)

let test_add_value () =
  let struct_items = Lesson12__Module_system.add_value "x" (Lesson12__Module_system.EConst 42) Lesson12__Module_system.empty_struct in
  check int "structure with value" 1 (List.length struct_items)

(* Signature checking tests *)

let test_check_structure_pass () =
  let sig_items = [
    Lesson12__Module_system.SigVal ("x", Lesson12__Module_system.TyInt);
  ] in
  let struct_items = [
    Lesson12__Module_system.StructVal ("x", Lesson12__Module_system.EConst 42);
  ] in
  check bool "valid structure" true
    (try Lesson12__Module_system.check_structure sig_items struct_items; true
     with Lesson12__Module_system.ModuleError _ -> false)

let test_check_structure_fail_missing () =
  let sig_items = [
    Lesson12__Module_system.SigVal ("x", Lesson12__Module_system.TyInt);
    Lesson12__Module_system.SigVal ("y", Lesson12__Module_system.TyInt);
  ] in
  let struct_items = [
    Lesson12__Module_system.StructVal ("x", Lesson12__Module_system.EConst 42);
  ] in
  check bool "missing value" true
    (try Lesson12__Module_system.check_structure sig_items struct_items; false
     with Lesson12__Module_system.ModuleError _ -> true)

(* Functor tests *)

let test_functor_application () =
  let param_sig = [Lesson12__Module_system.SigType ("t", None)] in
  let body = [Lesson12__Module_system.StructType ("u", Some Lesson12__Module_system.TyInt)] in
  let fct = {
    Lesson12__Functor_mod.fname = "F";
    fparam = ("X", param_sig);
    fbody = body;
  } in
  let arg = [Lesson12__Module_system.StructType ("t", Some Lesson12__Module_system.TyBool)] in
  let result = Lesson12__Functor_mod.apply_functor fct arg in
  check int "functor result length" 1 (List.length result)

(* BST functor tests *)

let test_int_bst_empty () =
  check bool "empty is empty" true (Lesson12__Functor_examples.IntSet.is_empty Lesson12__Functor_examples.IntSet.empty)

let test_int_bst_add () =
  let s = Lesson12__Functor_examples.IntSet.add 5 Lesson12__Functor_examples.IntSet.empty in
  check bool "5 is in set" true (Lesson12__Functor_examples.IntSet.mem 5 s);
  check bool "3 is not in set" true (not (Lesson12__Functor_examples.IntSet.mem 3 s))

let test_int_bst_multiple () =
  let s = Lesson12__Functor_examples.IntSet.add 3 (Lesson12__Functor_examples.IntSet.add 5 (Lesson12__Functor_examples.IntSet.add 1 Lesson12__Functor_examples.IntSet.empty)) in
  check bool "1 in set" true (Lesson12__Functor_examples.IntSet.mem 1 s);
  check bool "3 in set" true (Lesson12__Functor_examples.IntSet.mem 3 s);
  check bool "5 in set" true (Lesson12__Functor_examples.IntSet.mem 5 s);
  check bool "7 not in set" true (not (Lesson12__Functor_examples.IntSet.mem 7 s))

let test_int_bst_to_list () =
  let s = Lesson12__Functor_examples.IntSet.add 3 (Lesson12__Functor_examples.IntSet.add 1 (Lesson12__Functor_examples.IntSet.add 5 (Lesson12__Functor_examples.IntSet.add 2 Lesson12__Functor_examples.IntSet.empty))) in
  check (list int) "sorted list" [1; 2; 3; 5] (Lesson12__Functor_examples.IntSet.to_list s)

let test_string_bst () =
  let s = Lesson12__Functor_examples.StringSet.add "banana" (Lesson12__Functor_examples.StringSet.add "apple" Lesson12__Functor_examples.StringSet.empty) in
  check bool "apple in set" true (Lesson12__Functor_examples.StringSet.mem "apple" s);
  check bool "banana in set" true (Lesson12__Functor_examples.StringSet.mem "banana" s);
  check bool "cherry not in set" true (not (Lesson12__Functor_examples.StringSet.mem "cherry" s))

(* Hash table tests *)

module IntHashTable = Lesson12__Functor_examples.MakeHashTable (Lesson12__Functor_examples.IntHashable)

let test_hashtable_empty () =
  let tbl = IntHashTable.empty () in
  check bool "key not in empty table" true
    (IntHashTable.find 42 tbl = None)

let test_hashtable_add_find () =
  let tbl = IntHashTable.add 42 "answer" (IntHashTable.empty ()) in
  check (option string) "find 42" (Some "answer")
    (IntHashTable.find 42 tbl)

let test_hashtable_update () =
  let tbl = IntHashTable.add 42 "old" (IntHashTable.empty ()) in
  let tbl = IntHashTable.add 42 "new" tbl in
  check (option string) "updated value" (Some "new")
    (IntHashTable.find 42 tbl)

let test_hashtable_remove () =
  let tbl = IntHashTable.add 42 "answer" (IntHashTable.empty ()) in
  let tbl = IntHashTable.remove 42 tbl in
  check bool "removed key not found" true
    (IntHashTable.find 42 tbl = None)

(* Monad tests *)

let test_option_return () =
  check bool "return 5" true
    (match Lesson12__Functor_examples.OptionMonad.return 5 with Some 5 -> true | _ -> false)

let test_option_bind () =
  let result = Lesson12__Functor_examples.OptionMonad.bind (Some 5) (fun x -> Some (x * 2)) in
  check bool "bind Some" true (result = Some 10);
  let result = Lesson12__Functor_examples.OptionMonad.bind None (fun x -> Some (x * 2)) in
  check bool "bind None" true (result = None)

let test_option_map () =
  check bool "map Some" true
    (Lesson12__Functor_examples.OptionMonad.map (( * ) 2) (Some 5) = Some 10);
  check bool "map None" true
    (Lesson12__Functor_examples.OptionMonad.map (( * ) 2) None = None)

let test_list_return () =
  check (list int) "return 5" [5] (Lesson12__Functor_examples.ListMonad.return 5)

let test_list_bind () =
  let result = Lesson12__Functor_examples.ListMonad.bind [1; 2; 3] (fun x -> [x; x * 2]) in
  check (list int) "bind list" [1; 2; 2; 4; 3; 6] result

let () =
  run "Lesson12: Modules and Functors" [
    ("Signatures", [
      ("empty signature length", `Quick, test_empty_sig);
      ("signature with type", `Quick, test_add_type);
      ("signature with val", `Quick, test_add_val);
      ("extended signature length", `Quick, test_extend_sig);
    ]);
    ("Structures", [
      ("empty structure length", `Quick, test_empty_struct);
      ("structure with value", `Quick, test_add_value);
    ]);
    ("Signature Checking", [
      ("valid structure", `Quick, test_check_structure_pass);
      ("missing value", `Quick, test_check_structure_fail_missing);
    ]);
    ("Functors", [
      ("functor result length", `Quick, test_functor_application);
    ]);
    ("BST Functor", [
      ("empty is empty", `Quick, test_int_bst_empty);
      ("5 is in set", `Quick, test_int_bst_add);
      ("1 in set", `Quick, test_int_bst_multiple);
      ("sorted list", `Quick, test_int_bst_to_list);
      ("apple in set", `Quick, test_string_bst);
    ]);
    ("Hash Table Functor", [
      ("key not in empty table", `Quick, test_hashtable_empty);
      ("find 42", `Quick, test_hashtable_add_find);
      ("updated value", `Quick, test_hashtable_update);
      ("removed key not found", `Quick, test_hashtable_remove);
    ]);
    ("Monads", [
      ("return 5", `Quick, test_option_return);
      ("bind Some", `Quick, test_option_bind);
      ("map Some", `Quick, test_option_map);
      ("return 5", `Quick, test_list_return);
      ("bind list", `Quick, test_list_bind);
    ]);
  ]
