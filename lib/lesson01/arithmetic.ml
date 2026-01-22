type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr

let rec eval env = function
  | Const n -> n
  | Add (e1, e2) -> eval env e1 + eval env e2
  | Mul (e1, e2) -> eval env e1 * eval env e2
  | Var x ->
      (try env x
       with Not_found -> raise (Invalid_argument ("Unbound: " ^ x)))
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' y = if y = x then v1 else env y in
      eval env' e2

let run e = eval (fun _ -> raise Not_found) e

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | Var x -> x
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2

let pp_expr fmt e =
  Format.pp_print_string fmt (string_of_expr e)

(* {1 Parsing} *)

type token =
  | TINT of int
  | TPLUS | TMUL
  | TLPAREN | TRPAREN
  | TLET | TIN | TIDENT of string
  | TEOF

exception Syntax_error of string

(* Simple lexer *)
let tokenize (s: string) : token list =
  let len = String.length s in
  let rec pos i =
    if i >= len then []
    else
      let c = s.[i] in
      match c with
      | ' ' | '\t' | '\n' | '\r' -> pos (i + 1)
      | '(' -> TLPAREN :: pos (i + 1)
      | ')' -> TRPAREN :: pos (i + 1)
      | '+' -> TPLUS :: pos (i + 1)
      | '*' -> TMUL :: pos (i + 1)
      | '=' -> TIDENT "=" :: pos (i + 1)
      | '0' .. '9' ->
          let j = ref i in
          while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
          TINT (int_of_string (String.sub s i (!j - i))) :: pos !j
      | 'a' .. 'z' ->
          let j = ref i in
          while !j < len && s.[!j] >= 'a' && s.[!j] <= 'z' do incr j done;
          let id = String.sub s i (!j - i) in
          (match id with
           | "let" -> TLET | "in" -> TIN | _ -> TIDENT id) :: pos !j
      | _ -> raise (Syntax_error ("Unexpected char: " ^ String.make 1 c))
  in
  pos 0 @ [TEOF]

(* Parser state *)
type parser = { tokens: token array; mutable pos: int }

let make_parser tokens = { tokens = Array.of_list tokens; pos = 0 }

let peek p =
  if p.pos >= Array.length p.tokens then TEOF
  else p.tokens.(p.pos)

let consume p =
  let tok = peek p in
  p.pos <- p.pos + 1;
  tok

let rec parse_expr p =
  match peek p with
  | TEOF -> raise (Syntax_error "Unexpected end")
  | _ -> parse_additive p

and parse_additive p =
  let left = parse_multiplicative p in
  match peek p with
  | TPLUS -> let _ = consume p in Add (left, parse_additive p)
  | _ -> left

and parse_multiplicative p =
  let left = parse_primary p in
  match peek p with
  | TMUL -> let _ = consume p in Mul (left, parse_multiplicative p)
  | _ -> left

and parse_primary p =
  match peek p with
  | TINT n -> let _ = consume p in Const n
  | TIDENT x -> let _ = consume p in Var x
  | TLPAREN ->
      let _ = consume p in
      let e = parse_expr p in
      (match peek p with
       | TRPAREN -> let _ = consume p in e
       | _ -> raise (Syntax_error "Expected ')'"))
  | TLET ->
      let _ = consume p in
      (match peek p with
       | TIDENT x ->
           let _ = consume p in
           (match peek p with
            | TIDENT "=" ->
                let _ = consume p in  (* skip "=" *)
                let e1 = parse_expr p in
                (match peek p with
                 | TIN ->
                     let _ = consume p in
                     let e2 = parse_expr p in
                     Let (x, e1, e2)
                 | _ -> raise (Syntax_error "Expected 'in'"))
            | _ -> raise (Syntax_error "Expected '='"))
       | _ -> raise (Syntax_error "Expected identifier after 'let'"))
  | _ -> raise (Syntax_error "Unexpected token")

let parse s =
  let tokens = tokenize s in
  let p = make_parser tokens in
  let e = parse_expr p in
  match peek p with
  | TEOF -> e
  | _ -> raise (Syntax_error "Unexpected trailing input")
