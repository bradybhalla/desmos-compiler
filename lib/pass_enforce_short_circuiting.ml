open! Core
open! Languages
open! Types
open C_style_frontend

(* TODO brady: for now we just don't allow non-first conditions that have function calls.
  (if (g x y) (
    ...
  ) elif (h x y) (
    ...
  )
needs to be nested so we have a place for the call to h. we also need to handle if expressions

need to be careful about
  - short ciruiting with and/or
  - condition in the expression only evaluated if the condition is true
  - next condition only evaluated if the condition is true

maybe the best way to do it is just fully expand any expression
that isn't pure


TODO: also want to make short circuiting with And/Or explicit. if needed they should be turned into new if statements.

 *)

(* NOTE: this is just temporary error checking code that doesn't do the expected thing*)

let rec expr_has_call : expr -> bool = function
  | Call _ -> true
  | Unit | Register _ | Num _ | Bool _ -> false
  | Add (a, b)
  | Sub (a, b)
  | Mult (a, b)
  | Div (a, b)
  | And (a, b)
  | Or (a, b)
  | Mod (a, b)
  | Compare (_, a, b) ->
      expr_has_call a || expr_has_call b
  | Not e -> expr_has_call e
  | If_expr { conds; default } ->
      List.exists conds ~f:(fun (cond, e) ->
          expr_has_call cond || expr_has_call e)
      || expr_has_call default

let check_expr_no_calls expr =
  if expr_has_call expr then
    failwith
      "function call in expression position where calls are not allowed \
       (non-first if condition or if_expr condition)"

let rec check_stmt = function
  | Function_def (_, _, body) -> List.iter body ~f:check_stmt
  | Return e -> check_expr_no_calls e
  | Set (_, e) -> check_expr_no_calls e
  | Call _ -> ()
  | While (cond, body) ->
      check_expr_no_calls cond;
      List.iter body ~f:check_stmt
  | If { branches; else_ } ->
      (match branches with
      | [] -> ()
      | (_, first_body) :: rest ->
          (* First condition may have calls — only check its body stmts *)
          List.iter first_body ~f:check_stmt;
          (* Remaining conditions must not have calls in their condition expr *)
          List.iter rest ~f:(fun (cond, body) ->
              check_expr_no_calls cond;
              List.iter body ~f:check_stmt));
      List.iter else_ ~f:check_stmt

let compile (program : t) : t = program
(* List.iter program ~f:check_stmt; *)
(* program *)
