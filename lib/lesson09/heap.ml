(* {1 Configuration} *)

let heap_size = 100
let max_objects = 50

(* {1 Object Representation} *)

type header = {
  mutable marked : bool;
  size : int;
  tag : int;
}

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * value list
  | VUnit

and obj = {
  hdr : header;
  data : value array;
  mutable forward : obj option;
}

(* {1 Heap State} *)

type heap = {
  mutable objects : obj option array;
  mutable alloc_ptr : int;
  mutable size : int;
}

(* {1 Heap Operations} *)

let create_heap () =
  {
    objects = Array.make max_objects None;
    alloc_ptr = 0;
    size = 0;
  }

let alloc h size tag =
  if h.alloc_ptr >= max_objects then
    None
  else if h.alloc_ptr + size > max_objects then
    None
  else
    let obj = {
      hdr = { marked = false; size; tag };
      data = Array.make size VUnit;
      forward = None;
    } in
    h.objects.(h.alloc_ptr) <- Some obj;
    h.alloc_ptr <- h.alloc_ptr + 1;
    h.size <- h.size + 1;
    Some obj

let is_full h =
  h.alloc_ptr >= max_objects

let clear_marks h =
  for i = 0 to max_objects - 1 do
    match h.objects.(i) with
    | Some obj -> obj.hdr.marked <- false
    | None -> ()
  done

(* Get all objects referenced by a value *)
let value_refs _v =
  (* Simplified - would extract object references from values *)
  []

(* Get roots from stack and globals *)
let get_roots (_stack : value list) (_globals : value list array) =
  (* Simplified: we'll track objects directly *)
  []

(* {1 Pretty Printing} *)

let string_of_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure (x, _) -> "<λ" ^ x ^ ". ...>"
  | VUnit -> "()"

let string_of_obj obj =
  let contents = Array.map string_of_value obj.data
  |> Array.to_list
  |> String.concat ", " in
  Printf.sprintf "{marked=%b; size=%d; data=[%s]}"
    obj.hdr.marked obj.hdr.size contents

let string_of_heap h =
  let live = ref 0 in
  let rec aux_filter_map i acc =
    if i < 0 then acc
    else
      let acc' = match h.objects.(i) with
        | Some obj ->
            incr live;
            (string_of_obj obj) :: acc
        | None -> acc
      in
      aux_filter_map (i - 1) acc'
  in
  let objs = aux_filter_map (Array.length h.objects - 1) [] in
  Printf.sprintf "Heap[%d/%d]:\n  %s"
    !live max_objects
    (String.concat "\n  " objs)

let heap_stats h =
  Printf.sprintf "Heap: %d/%d objects allocated (%.1f%% full)"
    h.size max_objects
    (100.0 *. float h.size /. float max_objects)
