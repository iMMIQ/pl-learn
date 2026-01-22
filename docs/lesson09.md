# Lesson 09: Garbage Collection

**Learning Objectives:**
- Understand automatic memory management
- Learn mark-sweep collection
- Learn copying collection
- Understand generational GC
- Measure GC performance

## What is Garbage Collection?

**Garbage Collection (GC)** automatically reclaims memory that is no longer reachable.

### The Memory Management Problem

```c
/* Manual memory management (error-prone) */
char* buffer = malloc(1024);
// ... use buffer ...
free(buffer);  // Easy to forget!
```

```java
/* Automatic memory management (GC) */
String s = new String("hello");
// ... use s ...
// No free needed - GC handles it!
```

## Key Question: When is Memory "Garbage"?

**Answer:** Memory is **live** if it's reachable from the **roots**. Everything else is garbage.

### Roots

Roots are the starting points for reachability:

- **Stack variables**: Local variables in function calls
- **Global variables**: Static variables, module-level bindings
- **Registers**: CPU registers holding references

```
roots = {stack, globals, registers}
live = reachable(roots)
garbage = heap \ live
```

## Mark-Sweep Collection

The simplest GC algorithm, invented in 1960.

### Algorithm

```
Mark-Sweep():
  1. Mark phase:
     - Clear all mark bits
     - For each root:
       - Mark(root)

  2. Sweep phase:
     - For each object in heap:
       - If not marked: free it
       - Else: clear mark bit
```

### Mark

```ocaml
let rec mark obj =
  if obj.marked then ()  (* Already visited *)
  else
    obj.marked <- true;
    (* Recursively mark references *)
    for each ref in obj do
      mark ref
```

### Sweep

```ocaml
let sweep () =
  for each obj in heap do
    if not obj.marked then
      free obj
    else
      obj.marked <- false
```

### Complexity

| Aspect | Complexity |
|--------|------------|
| Time | O(heap_size) |
| Space | O(1) additional |
| Fragmentation | Can occur |

### Pros and Cons

✅ Simple
✅ No extra space needed
❌ Visits all objects (live + dead)
❌ Fragmentation

## Copying Collection

Divide heap into two semi-spaces and copy live objects.

### Algorithm

```
Copy():
  1. Allocate in from-space
  2. When from-space is full:
     - Copy all live objects to to-space
     - Swap from/to spaces
     - Continue allocating
```

### Forwarding

Each object needs a **forward pointer** to its new location:

```
original.forward = copy_in_to_space(original)
```

### Complexity

| Aspect | Complexity |
|--------|------------|
| Time | O(live_data) |
| Space | 2x heap (50% usage) |
| Fragmentation | None (compact) |

### Pros and Cons

✅ Only visits live objects
✅ Compacts (no fragmentation)
✅ Allocation is cheap (bump pointer)
❌ Uses 2x space
❌ Copies all objects every GC

## Generational Collection

**Key insight:** Most objects die young.

### Generational Hypothesis

> "Most objects die young; old objects rarely reference young objects."

### Strategy

Divide heap into **generations**:

```
┌────────────────────┐
│  Old Generation    │  (rarely collected)
├────────────────────┤
│  Young Generation  │  (frequently collected)
└────────────────────┘
```

### Collection

- **Minor GC**: Collect only young generation (fast, frequent)
- **Major GC**: Collect entire heap (slow, rare)

### Write Barrier

Track references from old to young objects:

```ocaml
(* Write barrier - pseudo-code *)
let set_field obj field value =
  obj.(field) <- value;
  if is_old obj && is_young value then
    remember_reference obj value
```

## GC in Real Languages

| Language | GC Algorithm | Notes |
|----------|--------------|-------|
| Go | Mark-sweep + tri-color | Concurrent GC |
| Java | Generational + compaction | G1 GC, ZGC |
| OCaml | Major + minor | Stop-the-world |
| Python | Reference counting + GC | Hybrid |
| Ruby | Tri-color marking | RGenGC |
| JavaScript | Generational | V8, SpiderMonkey |

## Visualization

### Mark-Sweep Example

```
Before GC:
Heap: [A]───→[B]───→[C]
       │
       └───→[D]

Roots: {A}

After Mark:
Marked: A, B, C, D  (all reachable)

After Sweep:
Heap: [A]───→[B]───→[C]
       │
       └───→[D]
(Nothing freed - all live)
```

### Copying Example

```
Before GC (From-space):
[A] [B] [C] [D]
  ↑   ↑   ↑
  └───┴───┴───> reachable

After GC (To-space):
[A'] [B'] [C']  (compacted)

[D] is abandoned (garbage)
```

## Try It

```ocaml
open Heap;;
open Mark_sweep;;

(* Create heap and allocate *)
let h = create_heap ();;
let o1 = alloc h 1 0 |> Option.get;;
let o2 = alloc h 1 0 |> Option.get;;

(* GC with no roots *)
mark_and_sweep h [];;  (* Frees both *)

(* GC with roots *)
let o3 = alloc h 1 0 |> Option.get;;
mark_and_sweep h [o3];;  (* Keeps o3 *)
```

## Exercises

1. Implement tri-color marking (for concurrent GC)
2. Add compaction to mark-sweep
3. Implement a generational collector
4. Add large object space (LOS)
5. Measure pause times for each algorithm

## Further Reading

- *Garbage Collection: Algorithms for Automatic Dynamic Memory Management*, Jones & Lins
- [The Garbage Collection Handbook](http://gchandbook.org/)
- [Go GC Implementation](https://go.dev/doc/gc-guide)

## Real-World Connection

- **OCaml**: Mark-sweep + generational
- **Java**: G1 collector (regional, concurrent)
- **Go**: Tri-color marking with write barrier
- **Python**: Reference counting + cycle detector

## Course Complete!

Congratulations! You've completed 9 lessons covering:
- AST and evaluation
- Lambda calculus
- Simply typed λ-calculus
- Operational semantics
- Continuations and CPS
- Type inference
- Abstract machines
- Subtyping
- Garbage collection

**What's next?**
- Study a real compiler (OCaml, LLVM)
- Explore dependent types
- Learn about linear types
- Build your own language!
