open! Core
open! Languages
open! Types

let compile_main =
  let rec (compile_expr
            : Register_stack_instrs.expr -> Desmos_virtual_machine.expr) =
    function
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
  in

  let compile_generalized_set_action = function
    | Register_stack_instrs.Set expr ->
        Desmos_virtual_machine.Set (compile_expr expr)
    | PushAndSet expr -> PushAndSet (compile_expr expr)
    | Push -> Push
    | Pop -> Pop
  in

  let compile_stmt =
    let open Desmos_virtual_machine in
    function
    | Register_stack_instrs.GeneralizedSet set_actions ->
        let sets =
          List.map set_actions ~f:(fun (reg, action) ->
              (reg, compile_generalized_set_action action))
        in
        Instruction (sets, NextInstr)
    | Jump { conds; default } ->
        let conds =
          List.map conds ~f:(fun (expr, lbl) ->
              (compile_expr expr, JumpToLabel lbl))
        in
        Instruction ([], Jump { conds; default = JumpToLabel default })
    | Label lbl -> Label lbl
    | Link_push_jump lbl ->
        (* TODO brady: If we made this push a label instead then we could jump
         to something like (link_register, PushAndSet (LabelVal lbl)). maybe
         this would be better? *)
        Instruction
          ( [ (link_register, PushAndSet (Add (ProgramCounter, Num 1.))) ],
            Jump { conds = []; default = JumpToLabel lbl } )
    | Link_pop_jump ->
        Instruction
          ( [ (link_register, Pop) ],
            Jump { conds = []; default = JumpToRegister link_register } )
  in
  List.map ~f:compile_stmt

let extract_registers =
  let open Desmos_virtual_machine in
  let rec registers_in_expr = function
    | Register r -> Register.Set.singleton r
    | ProgramCounter | Num _ | Bool _ -> Register.Set.empty
    | Add (a, b) | Sub (a, b) | Mult (a, b) | Div (a, b) | And (a, b) | Or (a, b)
      ->
        Set.union (registers_in_expr a) (registers_in_expr b)
    | Not e -> registers_in_expr e
  in

  let registers_in_jump_target = function
    | JumpToRegister r -> Register.Set.singleton r
    | JumpToLabel _ -> Register.Set.empty
  in

  let registers_in_pc_action = function
    | NextInstr -> Register.Set.empty
    | Jump { conds; default } ->
        let cond_regs =
          List.map conds ~f:(fun (expr, target) ->
              Set.union (registers_in_expr expr)
                (registers_in_jump_target target))
          |> Register.Set.union_list
        in
        Set.union cond_regs (registers_in_jump_target default)
  in

  let registers_in_stmt = function
    | Label _ -> Register.Set.empty
    | Instruction (sets, pc_action) ->
        let set_regs =
          List.map sets ~f:(fun (reg, action) ->
              let expr_regs =
                match action with
                | Set expr | PushAndSet expr -> registers_in_expr expr
                | Push | Pop -> Register.Set.empty
              in
              Set.add expr_regs reg)
          |> Register.Set.union_list
        in
        Set.union set_regs (registers_in_pc_action pc_action)
  in

  fun stmts -> List.map stmts ~f:registers_in_stmt |> Register.Set.union_list

let compile program =
  let main = compile_main program in
  let registers = extract_registers main in
  { Desmos_virtual_machine.main; registers }
