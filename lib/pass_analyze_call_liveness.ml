open! Core
open! Languages
open! Types
module Rset = Register.Set

(* TODO brady: eventually we can determine which registers a function will actually clobber and add that to the Calls as well. We can also compute actual liveness and narrow down the set of saved registers to be as small as possible.  *)

let rec uncover_register_expr = function
  | Register_func_instrs.Register r -> Rset.singleton r
  | Num _ | Bool _ -> Rset.empty
  | Add (a, b)
  | Sub (a, b)
  | Mult (a, b)
  | Div (a, b)
  | And (a, b)
  | Or (a, b)
  | Mod (a, b) ->
      Set.union (uncover_register_expr a) (uncover_register_expr b)
  | Not e -> uncover_register_expr e
  | If_expr { conds; default } ->
      let cond_regs =
        List.map conds ~f:(fun (cond, e) ->
            Set.union
              (uncover_register_condition cond)
              (uncover_register_expr e))
        |> Register.Set.union_list
      in
      Set.union cond_regs (uncover_register_expr default)

and uncover_register_condition = function
  | Register_func_instrs.Compare (_, a, b) ->
      Set.union (uncover_register_expr a) (uncover_register_expr b)
  | BoolVal e -> uncover_register_expr e

let uncover_registers_control_flow = function
  | Register_func_instrs.Jump { conds; default = _ } ->
      conds
      |> List.map ~f:(fun (cond, _) -> uncover_register_condition cond)
      |> List.fold ~f:Set.union ~init:Rset.empty
  | Return expr -> uncover_register_expr expr
  | Exit -> Rset.empty

let uncover_registers_stmt = function
  | Register_func_instrs.Set (reg, expr) ->
      Set.union (Rset.singleton reg) (uncover_register_expr expr)
  | Call { func_name = _; args; ret } ->
      let args_regs =
        args
        |> List.map ~f:uncover_register_expr
        |> List.fold ~f:Set.union ~init:Rset.empty
      in
      let ret_regs =
        Option.value_map ret ~default:Rset.empty ~f:Rset.singleton
      in
      Set.union args_regs ret_regs

let uncover_registers_blocks blocks =
  blocks
  |> List.concat_map ~f:(fun (b : Register_func_instrs.block) ->
         List.map b.body ~f:uncover_registers_stmt
         @ [ uncover_registers_control_flow b.control_flow ])
  |> List.fold ~f:Set.union ~init:Rset.empty

let compile_stmt ~live_registers = function
  | Register_func_instrs.Set (reg, expr) ->
      Register_func_instrs_with_call_liveness.Set (reg, expr)
  | Call { func_name; args; ret } ->
      Call { func_name; args; ret; live_registers }

let compile_block ~live_registers (block : Register_func_instrs.block) =
  {
    Register_func_instrs_with_call_liveness.label = block.label;
    body = List.map block.body ~f:(compile_stmt ~live_registers);
    control_flow = block.control_flow;
  }

let compile { Register_func_instrs.functions; main } =
  let compiled_functions =
    Map.mapi functions ~f:(fun ~key:_ ~data:def ->
        let live_registers =
          Set.union (Rset.of_list def.params)
            (uncover_registers_blocks def.blocks)
          |> Set.to_list
        in
        {
          Register_func_instrs_with_call_liveness.entry_label = def.entry_label;
          params = def.params;
          blocks = List.map def.blocks ~f:(compile_block ~live_registers);
        })
  in
  let main_live_registers = uncover_registers_blocks main |> Set.to_list in
  {
    Register_func_instrs_with_call_liveness.functions = compiled_functions;
    main = List.map main ~f:(compile_block ~live_registers:main_live_registers);
  }
