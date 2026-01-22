(* {1 Maybe Monad} *)

module Maybe = struct
  type 'a t =
    | Just of 'a
    | Nothing

  let return x = Just x

  let bind = function
    | Nothing -> fun _ -> Nothing
    | Just x -> fun f -> f x

  let map f = function
    | Nothing -> Nothing
    | Just x -> Just (f x)

  let from_option = function
    | Some x -> Just x
    | None -> Nothing

  let to_option = function
    | Just x -> Some x
    | Nothing -> None
end

(* {1 Exception Monad (Result) } *)

module Result = struct
  type ('e, 'a) t =
    | Ok of 'a
    | Error of 'e

  let return x = Ok x

  let bind = function
    | Error e -> fun _ -> Error e
    | Ok x -> fun f -> f x

  let map f = function
    | Error e -> Error e
    | Ok x -> Ok (f x)

  let throw e = Error e

  let catch m handler = match m with
    | Error e -> handler e
    | Ok x -> Ok x

  let attempt f =
    try Ok (f ())
    with e -> Error e

  let lift_option e = function
    | Some x -> Ok x
    | None -> Error e

  let option_to_error e = function
    | Some x -> Ok x
    | None -> Error e
end

(* {1 Validation Applicative} *)

module Validation = struct
  type ('e, 'a) t =
    | Valid of 'a
    | Invalid of 'e list

  let return x = Valid x

  let map f = function
    | Invalid es -> Invalid es
    | Valid x -> Valid (f x)

  let ap vf vx = match vf, vx with
    | Invalid e1, Invalid e2 -> Invalid (e1 @ e2)
    | Invalid e1, Valid _ -> Invalid e1
    | Valid _, Invalid e2 -> Invalid e2
    | Valid f, Valid x -> Valid (f x)

  let validate combine validations =
    let rec folder acc = function
      | [] -> begin
          match acc with
          | Invalid _ -> acc
          | Valid xs -> Valid (List.rev xs)
        end
      | Valid x :: rest ->
          folder (map (fun xs -> x :: xs) acc) rest
      | Invalid es :: rest ->
        match acc with
        | Invalid es' -> folder (Invalid (es @ es')) rest
        | Valid _ -> folder (Invalid es) rest
    in
    match folder (Valid []) validations with
    | Valid xs -> Valid (List.rev xs)
    | Invalid es -> Invalid (combine es)
end

(* {1 Compatibility aliases} *)

type 'a maybe = 'a Maybe.t =
  | Just of 'a
  | Nothing

type ('e, 'a) result = ('e, 'a) Result.t =
  | Ok of 'a
  | Error of 'e

type ('e, 'a) validation = ('e, 'a) Validation.t =
  | Valid of 'a
  | Invalid of 'e list

[@@@warning "-32"]

let return = Maybe.return
let bind = Maybe.bind
let map = Maybe.map
let from_option = Maybe.from_option
let to_option = Maybe.to_option

(* Expose constructors for compatibility *)
let throw = Result.throw
let catch = Result.catch
let attempt = Result.attempt
let lift_option = Result.lift_option
let option_to_error = Result.option_to_error
let ap = Validation.ap
let validate = Validation.validate
let return_valid = Validation.return
let map_valid = Validation.map
