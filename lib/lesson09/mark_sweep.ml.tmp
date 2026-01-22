open Heap

(* {1 Mark Phase} *)

let mark _h obj =
  if obj.hdr.marked then
    ()  (* Already marked *)
  else begin
    (* Mark this object *)
    obj.hdr.marked <- true;

    (* Recursively mark referenced objects *)
    Array.iter (function
      | VInt _ | VBool _ | VUnit -> ()
      | VClosure (_, env) ->
          (* Mark objects in environment *)
          List.iter (fun v ->
            match v with
            | VInt _ | VBool _ | VUnit -> ()
            | VClosure _ -> ()  (* Simplified *)
          ) env
    ) obj.data
  end

let mark_phase h roots =
  List.iter (mark h) roots

(* {1 Sweep Phase} *)

let sweep h =
  let freed = ref 0 in
  for i = 0 to max_objects - 1 do
    match h.objects.(i) with
    | Some obj when not obj.hdr.marked ->
        (* Free this object *)
        h.objects.(i) <- None;
        h.size <- h.size - 1;
        incr freed
    | Some obj ->
        obj.hdr.marked <- false  (* Reset for next GC *)
    | None -> ()
  done;
  !freed

let mark_and_sweep h roots =
  mark_phase h roots;
  sweep h
