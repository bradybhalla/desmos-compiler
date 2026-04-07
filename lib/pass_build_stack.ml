open! Core
open! Languages
open! Types
module Rset = Register.Set

let uncover_register_expr = function _ -> failwith "TODO"

(* TODO brady: move the uncover functions to another pass that just pulls out all registers (and maybe function deps or other things we care about for this pass) *)
let uncover_registers_stmt = function
  | Register_func_instrs.Set (reg, expr) ->
      Set.union (Rset.singleton reg) (uncover_register_expr expr)
  | Jump { conds; default = _ } ->
      conds |> List.map ~f:fst
      |> List.map ~f:uncover_register_expr
      |> List.fold ~f:Set.union ~init:Rset.empty
  | _ -> failwith "TODO"

let uncover_registers_func
    ({ params = _; body = _; name = _ } : Register_func_instrs.function_def) =
  failwith "TODO"

let compile (_input : Register_func_instrs.t) : Register_stack_instrs.t =
  failwith "TODO"
