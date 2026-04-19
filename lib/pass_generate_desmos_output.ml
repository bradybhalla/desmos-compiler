open! Core
open! Languages
open! Types
open Desmos_output

(* TODO brady: split comparisons from normal expressions here and in previous languages. *)
let rec compile_expr : Desmos_virtual_machine.expr -> expr = function
  | Desmos_virtual_machine.Register r -> Register r
  | ProgramCounter -> ProgramCounter
  | Num n -> Num n
  | Bool b -> Num (if b then 1. else 0.)
  | Add (a, b) -> Add (compile_expr a, compile_expr b)
  | Sub (a, b) -> Sub (compile_expr a, compile_expr b)
  | Mult (a, b) -> Mult (compile_expr a, compile_expr b)
  | Div (a, b) -> Div (compile_expr a, compile_expr b)
  | And (a, b) -> And (compile_expr a, compile_expr b)
  | Or (a, b) -> Or (compile_expr a, compile_expr b)
  | Not e -> Not (compile_expr e)
  | Mod (a, b) -> Mod (compile_expr a, compile_expr b)
  | Compare (_, _, _) ->
      failwith
        "Compare not allowed in arithmetic expression context, this should be \
         refactored later"

let compile_jump_cond : Desmos_virtual_machine.expr -> condition = function
  | Desmos_virtual_machine.Compare (op, a, b) ->
      Compare (op, compile_expr a, compile_expr b)
  | e -> BoolVal (compile_expr e)

let compile_jump_target label_map : Desmos_virtual_machine.jump_target -> expr =
  function
  | Desmos_virtual_machine.JumpToLabel lbl ->
      Num (Float.of_int (Map.find_exn label_map lbl))
  | JumpToRegister r -> Register r

let compile_generalized_set (reg : Register.t)
    (action : Desmos_virtual_machine.generalized_set) : set list =
  let stack_reg = get_stack_register reg in
  match action with
  | Desmos_virtual_machine.Set expr -> [ (reg, compile_expr expr) ]
  | PushAndSet expr ->
      [
        (stack_reg, ListJoin (Register stack_reg, Register reg));
        (reg, compile_expr expr);
      ]
  | Push -> [ (stack_reg, ListJoin (Register stack_reg, Register reg)) ]
  | Pop ->
      [
        (reg, ListIndex (Register stack_reg, ListLength (Register stack_reg)));
        ( stack_reg,
          ListSlice
            ( Register stack_reg,
              Num 1.,
              Sub (ListLength (Register stack_reg), Num 1.) ) );
      ]

let build_label_map stmts =
  let _, label_map =
    List.fold_left stmts ~init:(0, Label.Map.empty) ~f:(fun (idx, m) stmt ->
        match stmt with
        | Desmos_virtual_machine.Label lbl ->
            (idx, Map.add_exn m ~key:lbl ~data:idx)
        | Instruction _ -> (idx + 1, m))
  in
  label_map

let extract_instructions stmts =
  List.filter_map stmts ~f:(function
    | Desmos_virtual_machine.Label _ -> None
    | Instruction i -> Some i)

let compile_instruction label_map idx (sets, pc_action) =
  let pc_eq_i : condition =
    Compare (Compare_op.Eq, ProgramCounter, Num (Float.of_int idx))
  in
  let reg_sets =
    List.concat_map sets ~f:(fun (reg, action) ->
        compile_generalized_set reg action)
  in
  match pc_action with
  | Desmos_virtual_machine.NextInstr ->
      let pc_set = (program_counter_reg, Add (ProgramCounter, Num 1.)) in
      [ (pc_eq_i, reg_sets @ [ pc_set ]) ]
  | Exit ->
      let pc_set = (program_counter_reg, Num (-1.)) in
      [ (pc_eq_i, reg_sets @ [ pc_set ]) ]
  | Jump { conds; default } ->
      let pc_expr =
        If_expr
          {
            conds =
              List.map conds ~f:(fun (cond_expr, target) ->
                  ( compile_jump_cond cond_expr,
                    compile_jump_target label_map target ));
            default = compile_jump_target label_map default;
          }
      in
      [ (pc_eq_i, reg_sets @ [ (program_counter_reg, pc_expr) ]) ]

let compile (program : Desmos_virtual_machine.t) : t =
  let label_map = build_label_map program.main in
  let instrs = extract_instructions program.main in
  let conds =
    List.concat_mapi instrs ~f:(fun idx instr ->
        compile_instruction label_map idx instr)
  in
  let default_sets = [ (program_counter_reg, Num (-1.)) ] in
  let program_action = { conds; default = default_sets } in
  let init_pc = (program_counter_reg, Num 0.) in
  let init_registers =
    init_pc
    :: List.concat_map (Set.to_list program.registers) ~f:(fun reg ->
           (* TODO brady: right now magic numbers will show if something goes wrong.
              Every stack starts with an element so valid code should never be able
              to remove it. This is also useful because our current "pop" function
              doesn't work for lists of size 0/1. *)
           [
             (reg, Num 5.4321);
             (get_stack_register reg, ListLiteral [ Num 1.2345 ]);
           ])
  in
  { program_action; init_registers }
