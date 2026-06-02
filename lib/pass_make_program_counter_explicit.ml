open! Core
open Languages
open Types
open Desmos_virtual_machine

let rec compile_expr : Register_stack_instrs.expr -> Desmos_virtual_machine.expr
    = function
  | Register r -> Register r
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
  | JumpLink { target; return_label } ->
      Instruction
        [
          ( program_counter_reg,
            PushExprAndSet
              {
                push = LabelLineNumber return_label;
                set = LabelLineNumber target;
              } );
        ]
  | Return expr ->
      Instruction
        [
          (return_register, Set (compile_expr expr)); (program_counter_reg, Pop);
        ]
  | Exit -> Desmos_virtual_machine.Exit

let compile_block (block : Register_stack_instrs.block) =
  let body = List.map block.body ~f:compile_body_stmt in
  let control_flow = compile_control_flow block.control_flow in
  { Desmos_virtual_machine.label = block.label; body = body @ [ control_flow ] }

let compile_desmos_plot = function
  | Register_func_instrs.Point { x; y; args } ->
      Desmos_virtual_machine.Point
        { x = compile_expr x; y = compile_expr y; args }
  | Line { x1; y1; x2; y2; args } ->
      Line
        {
          x1 = compile_expr x1;
          y1 = compile_expr y1;
          x2 = compile_expr x2;
          y2 = compile_expr y2;
          args;
        }

let compile { Register_stack_instrs.blocks; registers; desmos_vars;
              desmos_plots } =
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
    desmos_vars;
    desmos_plots = List.map desmos_plots ~f:compile_desmos_plot;
  }
