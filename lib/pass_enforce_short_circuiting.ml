open! Core
open! Languages
open! Types
open! C_style_frontend

(* TODO brady: maybe it makes sense to merge this with the extract function calls and defs pass. they have almost identical structure. *)

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

TODO: make sure to check a huge nested one



(f(x) and f(y)) or f(z)

if (a and b)
{{ f(x) : f(y), false  } : true, f(z)}


tests:
  - and/or get turned into if
  - ifexpr get turned into if
 *)

(* let register_gen = Register_generator.create "0short_circuit" *)
(**)
(* let rec expr_contains_call = function *)
(*   | Unit | Register _ | Num _ | Bool _ -> false *)
(*   | Add (e1, e2) *)
(*   | Sub (e1, e2) *)
(*   | Mult (e1, e2) *)
(*   | Div (e1, e2) *)
(*   | Mod (e1, e2) *)
(*   | Compare (_, e1, e2) *)
(*   | And (e1, e2) *)
(*   | Or (e1, e2) -> *)
(*       expr_contains_call e1 || expr_contains_call e2 *)
(*   | Not e -> expr_contains_call e *)
(*   | If_expr { conds; default } -> *)
(*       List.exists conds ~f:(fun (cond, result) -> *)
(*           expr_contains_call cond || expr_contains_call result) *)
(*       || expr_contains_call default *)
(*   | Call _ -> true *)
(**)
(* let rec compile_expr = function *)
(*   | Unit -> ([], Unit) *)
(*   | Register reg -> ([], Register reg) *)
(*   | Num n -> ([], Num n) *)
(*   | Bool b -> ([], Bool b) *)
(*   | Add (e1, e2) -> *)
(*       let stmts1, e1 = compile_expr e1 in *)
(*       let stmts2, e2 = compile_expr e2 in *)
(*       (stmts1 @ stmts2, Add (e1, e2)) *)
(*   | Sub (e1, e2) -> *)
(*       let stmts1, e1 = compile_expr e1 in *)
(*       let stmts2, e2 = compile_expr e2 in *)
(*       (stmts1 @ stmts2, Sub (e1, e2)) *)
(*   | Mult (e1, e2) -> *)
(*       let stmts1, e1 = compile_expr e1 in *)
(*       let stmts2, e2 = compile_expr e2 in *)
(*       (stmts1 @ stmts2, Mult (e1, e2)) *)
(*   | Div (e1, e2) -> *)
(*       let stmts1, e1 = compile_expr e1 in *)
(*       let stmts2, e2 = compile_expr e2 in *)
(*       (stmts1 @ stmts2, Div (e1, e2)) *)
(*   | Mod (e1, e2) -> *)
(*       let stmts1, e1 = compile_expr e1 in *)
(*       let stmts2, e2 = compile_expr e2 in *)
(*       (stmts1 @ stmts2, Mod (e1, e2)) *)
(*   | Compare (op, e1, e2) -> *)
(*       let stmts1, e1 = compile_expr e1 in *)
(*       let stmts2, e2 = compile_expr e2 in *)
(*       (stmts1 @ stmts2, Compare (op, e1, e2)) *)
(*   | Not e -> *)
(*       let stmts, e = compile_expr e in *)
(*       (stmts, Not e) *)
(*   | Call (func_name, args) -> *)
(*       let stmts, args = args |> List.map ~f:compile_expr |> List.unzip in *)
(*       (List.concat stmts, Call (func_name, args)) *)
(*   (* TODO: we could just get rid of and/or and always treat it as if expressions. its possible that could make it slower since I'm guessing a desmos compare is slow compared to a mutliply, but maybe its worth testing. *) *)
(*   | And (e1, e2) as e -> *)
(*       if expr_contains_call e then *)
(*         (* delegate to If_expr *) *)
(*         compile_expr (If_expr { conds = [ (e1, e2) ]; default = Bool false }) *)
(*       else ([], e) *)
(*   | Or (e1, e2) as e -> *)
(*       if expr_contains_call e then *)
(*         (* delegate to If_expr *) *)
(*         compile_expr (If_expr { conds = [ (e1, Bool true) ]; default = e2 }) *)
(*       else ([], e) *)
(*   | If_expr { conds; default } as e -> *)
(*       if expr_contains_call e then *)
(*         (* delegate to if statement. the conds will be handled for us but TODO actually will they? *) *)
(*         let reg = Register_generator.generate register_gen in *)
(*         (* TODO need to finish this*) *)
(*         ( compile_stmt *)
(*             (If *)
(*                { *)
(*                  branches = []; *)
(*                  else_ = *)
(*                    (let stmts, default = compile_expr default in *)
(*                     1); *)
(*                }), *)
(*           Register reg ) *)
(*       else ([], e) *)
(**)
(* and compile_stmt = function *)
(*   | Function_def (name, params, body) -> *)
(*       [ Function_def (name, params, List.concat_map body ~f:compile_stmt) ] *)
(*   | Return expr -> *)
(*       let stmts, expr = compile_expr expr in *)
(*       stmts @ [ Return expr ] *)
(*   | Set (reg, expr) -> *)
(*       let stmts, expr = compile_expr expr in *)
(*       stmts @ [ Set (reg, expr) ] *)
(*   | Call (func_name, args) -> *)
(*       let stmts, args = args |> List.map ~f:compile_expr |> List.unzip in *)
(*       List.concat stmts @ [ Call (func_name, args) ] *)
(*   | While (cond, body) -> *)
(*       let stmts_before_cond, cond = compile_expr cond in *)
(*       stmts_before_cond *)
(*       @ [ *)
(*           While (cond, List.concat_map body ~f:compile_stmt @ stmts_before_cond); *)
(*         ] *)
(*   | If { branches; else_ } -> *)
(*       let rec build_short_circuited_if branches else_ acc_if_branches_rev = *)
(*         match branches with *)
(*         | [] -> *)
(*             [ *)
(*               If *)
(*                 { *)
(*                   branches = List.rev acc_if_branches_rev; *)
(*                   else_ = List.concat_map else_ ~f:compile_stmt; *)
(*                 }; *)
(*             ] *)
(*         | (cond, body) :: other_branches -> *)
(*             if expr_contains_call cond then *)
(*               let (cond_stmts, cond) = compile_expr cond in *)
(*             [ *)
(*               If *)
(*                 { *)
(*                   branches = List.rev acc_if_branches_rev; *)
(*                   else_ = List.concat_map else_ ~f:compile_stmt; *)
(*                 }; *)
(*             ] *)
(*             else *)
(*               build_short_circuited_if other_branches else_ *)
(*                 ((cond, compile_stmt body) :: acc_if_branches_rev) *)
(*       in *)
(*       build_short_circuited_if branches else_ [] *)
(**)
(* let compile program = *)
(*   Register_generator.reset register_gen; *)
(*   List.concat_map program ~f:compile_stmt *)
