(* {1 Maybe Monad} *)

[@@@warning "-32"]

type 'a maybe =
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

(* {1 Exception Monad (Result) } *)

type ('e, 'a) result =
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

(* {1 Validation Applicative} *)

type ('e, 'a) validation =
  | Valid of 'a
  | Invalid of 'e list

let return_valid x = Valid x

let map_valid f = function
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
        folder (map_valid (fun xs -> x :: xs) acc) rest
    | Invalid es :: rest ->
      match acc with
      | Invalid es' -> folder (Invalid (es @ es')) rest
      | Valid _ -> folder (Invalid es) rest
  in
  match folder (Valid []) validations with
  | Valid xs -> Valid (List.rev xs)
  | Invalid es -> Invalid (combine es)
