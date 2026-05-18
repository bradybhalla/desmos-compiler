open! Core
open Languages
open Types
open Desmos_virtual_machine

let rec compile_expr = function
  | Register_stack_instrs.Register r -> Register r
  | Num n -> Num n
  | Bool b -> Bool b
  | Add (a, b) -> Add (compile_expr a, compile_expr b)
  | Sub (a, b) -> Sub (compile_expr a, compile_expr b)
  | Mult (a, b) -> Mult (compile_expr a, compile_expr b)
  | Div (a, b) -> Div (compile_expr a, compile_expr b)
  | And (a, b) -> And (compile_expr a, compile_expr b)
  | Or (a, b) -> Or (compile_expr a, compile_expr b)
  | Not e -> Not (compile_expr e)
  | Mod (a, b) -> Mod (compile_expr a, compile_expr b)
  | Compare (op, a, b) -> Compare (op, compile_expr a, compile_expr b)
  | If_expr { conds; default } ->
      If_expr
        {
          conds =
            List.map conds ~f:(fun (cond, e) ->
                (compile_expr cond, compile_expr e));
          default = compile_expr default;
        }

let compile_generalized_set_action = function
  | Register_stack_instrs.Set expr -> Set (compile_expr expr)
  | PushAndSet expr -> PushAndSet (compile_expr expr)
  | Push -> Push
  | Pop -> Pop

let compile_body_stmt = function
  | Register_stack_instrs.GeneralizedSet set_actions ->
      let sets =
        List.map set_actions ~f:(fun (reg, action) ->
            (reg, compile_generalized_set_action action))
      in
      Instruction
        ((program_counter_reg, Set (Add (Register program_counter_reg, Num 1.)))
        :: sets)
  | JumpLink lbl ->
      Instruction
        [
          (link_register, Set (Add (Register program_counter_reg, Num 1.)));
          (program_counter_reg, Set (LabelLineNumber lbl));
        ]

let compile_control_flow = function
  | Register_stack_instrs.Jump { conds; default } ->
      let compiled_conds =
        List.map conds ~f:(fun (cond, lbl) ->
            (compile_expr cond, LabelLineNumber lbl))
      in
      Instruction
        [
          ( program_counter_reg,
            Set
              (If_expr
                 { conds = compiled_conds; default = LabelLineNumber default })
          );
        ]
  | Return expr ->
      Instruction
        [
          (return_register, Set (compile_expr expr));
          (program_counter_reg, Set (Register link_register));
        ]
  | Exit -> Desmos_virtual_machine.Exit

let compile_block (block : Register_stack_instrs.block) =
  let body = List.map block.body ~f:compile_body_stmt in
  let control_flow = compile_control_flow block.control_flow in
  { Desmos_virtual_machine.label = block.label; body = body @ [ control_flow ] }

let compile { Register_stack_instrs.blocks; registers } =
  let initial_registers =
    Set.add registers program_counter_reg
    |> Set.to_map ~f:(fun reg ->
           if Register.(reg = program_counter_reg) then
             Desmos_virtual_machine.Num 0.
             (* TODO important: have a better way to handle initial register values *)
           else Num 1.2345)
  in
  {
    Desmos_virtual_machine.main = List.map blocks ~f:compile_block;
    registers = initial_registers;
  }
