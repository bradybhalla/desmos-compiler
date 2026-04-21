open! Core
open! Languages
open! Types
open Desmos_virtual_machine

(* TODO brady: also add infomration about which registers actually need stacks *)
(* TODO brady: should I force 00pc to be included here? right now I automatically add it if necessary in the generate desmos output step *)

let rec registers_in_expr = function
  | Register r -> Register.Set.singleton r
  | LabelLineNumber _ | Num _ | Bool _ -> Register.Set.empty
  | Add (a, b)
  | Sub (a, b)
  | Mult (a, b)
  | Div (a, b)
  | And (a, b)
  | Or (a, b)
  | Mod (a, b) ->
      Set.union (registers_in_expr a) (registers_in_expr b)
  | Not e -> registers_in_expr e
  | If_expr { conds; default } ->
      let cond_regs =
        List.map conds ~f:(fun (cond, expr) ->
            Set.union (registers_in_condition cond) (registers_in_expr expr))
        |> Register.Set.union_list
      in
      Set.union cond_regs (registers_in_expr default)

and registers_in_condition = function
  | Compare (_, a, b) -> Set.union (registers_in_expr a) (registers_in_expr b)
  | BoolVal e -> registers_in_expr e

let registers_in_stmt = function
  | Label _ | Exit -> Register.Set.empty
  | Instruction sets ->
      List.map sets ~f:(fun (reg, action) ->
          let expr_regs =
            match action with
            | Set expr | PushAndSet expr -> registers_in_expr expr
            | Push | Pop -> Register.Set.empty
          in
          Set.add expr_regs reg)
      |> Register.Set.union_list

let compile (program : unit Desmos_virtual_machine.t) :
    Register.Set.t Desmos_virtual_machine.t =
  let registers =
    List.map program.main ~f:registers_in_stmt |> Register.Set.union_list
  in
  { program with info = registers }
