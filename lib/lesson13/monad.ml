(* {1 Monad Typeclass} *)

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val join : 'a t t -> 'a t
end

(* {1 Generic Monad Operations} *)

(* Note: These are polymorphic but OCaml doesn't have Haskell-style typeclasses.
   We define them as functors that take a Monad module. *)

module MonadUtils (M : Monad) = struct
  open M

  let rec mapM f = function
    | [] -> return []
    | x :: xs ->
        bind (f x) (fun y ->
        bind (mapM f xs) (fun ys ->
        return (y :: ys)))

  let rec filterM p = function
    | [] -> return []
    | x :: xs ->
        bind (p x) (fun b ->
        bind (filterM p xs) (fun ys ->
        return (if b then x :: ys else ys)))

  let rec foldM f init = function
    | [] -> return init
    | x :: xs ->
        bind (f init x) (fun acc ->
        foldM f acc xs)

  let rec sequence = function
    | [] -> return []
    | m :: ms ->
        bind m (fun x ->
        bind (sequence ms) (fun xs ->
        return (x :: xs)))

  let replicateM n m =
    let rec aux n acc =
      if n <= 0 then return (List.rev acc)
      else bind m (fun x -> aux (n - 1) (x :: acc))
    in
    aux n []
end

(* Kleisli composition operators - defined inline where needed for specific monads *)
