(* Re-open Monad to get the type *)
open Monad

[@@@warning "-32"]

(* {1 Common Transformer Interface} *)

module type MonadTrans = sig
  type 'a t
  type 'a inner

  val lift : 'a inner -> 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val join : 'a t t -> 'a t
end

(* {1 MaybeT Transformer} *)

module MaybeT (M : Monad) = struct
  type 'a t = 'a option M.t

  type 'a inner = 'a M.t

  let lift m = M.map (fun x -> Some x) m

  let return x = M.return (Some x)

  let bind m f = M.bind m (function
    | None -> M.return None
    | Some x -> f x)

  let map f m = bind m (fun x -> return (f x))

  let join mm = bind mm (fun m -> m)
end

(* {1 StateT Transformer} *)

module StateT (M : Monad) = struct
  type state
  type 'a t = {
    run : state -> ('a * state) M.t
  }

  type 'a inner = 'a M.t

  let lift m = { run = fun s -> M.bind m (fun x -> M.return (x, s)) }

  let return x = { run = fun s -> M.return (x, s) }

  let bind m f = { run = fun s ->
    M.bind (m.run s) (fun (x, s') ->
    (f x).run s')
  }

  let map f m = bind m (fun x -> return (f x))

  let join mm = bind mm (fun m -> m)

  let get = { run = fun s -> M.return (s, s) }

  let put s = { run = fun _ -> M.return ((), s) }

  let modify f = { run = fun s -> M.return ((), f s) }

  let run_state init m = m.run init
end

(* {1 ExceptT Transformer} *)

module ExceptT (M : Monad) = struct
  type error = string
  type 'a t = (string, 'a) Exception.result M.t

  type 'a inner = 'a M.t

  let lift m = M.map Exception.(return) m

  let return x = M.return Exception.(return x)

  let bind m f = M.bind m (function
    | Exception.Error e -> M.return (Exception.Error e)
    | Exception.Ok x -> f x)

  let map f m = bind m (fun x -> return (f x))

  let join mm = bind mm (fun m -> m)

  let throwE e = M.return (Exception.Error e)

  let catchE m handler = M.bind m (function
    | Exception.Error e -> handler e
    | Exception.Ok x -> M.return (Exception.Ok x))

  let run m = m
end

(* {1 Combined Effects} *)

module StateWithError = struct
  (* State transformer over the identity/result monad *)
  type state
  type 'a t = state -> (string, 'a * state) Exception.result

  let return x s = Exception.Ok (x, s)

  let bind m f s = match m s with
    | Exception.Error e -> Exception.Error e
    | Exception.Ok (x, s') -> f x s'

  let map f m = bind m (fun x -> return (f x))

  let throw e _ = Exception.Error e

  let get s = Exception.Ok (s, s)

  let put s' _ = Exception.Ok ((), s')

  let run init m = m init
end

module StateWithLog = struct
  type state
  type 'a t = state -> string list -> 'a * state * string list

  let return x s logs = (x, s, logs)

  let bind m f s logs =
    let (x, s', logs') = m s logs in
    f x s' logs'

  let map f m = bind m (fun x -> return (f x))

  let log msg s logs = ((), s, msg :: logs)

  let get s logs = (s, s, logs)

  let put s' _ logs = ((), s', logs)

  let run init m = m init []
end
