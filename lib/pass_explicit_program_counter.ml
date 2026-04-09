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
