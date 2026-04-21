open! Core
open! Languages
open! Types
open Desmos_output

let build_label_map stmts =
  let _, label_map =
    List.fold_left stmts ~init:(0, Label.Map.empty) ~f:(fun (idx, m) stmt ->
        match stmt with
        | Desmos_virtual_machine.Label lbl ->
            (idx, Map.add_exn m ~key:lbl ~data:idx)
        | Instruction _ | Exit -> (idx + 1, m))
  in
  label_map

let rec compile_expr label_map : Desmos_virtual_machine.expr -> expr = function
  | Desmos_virtual_machine.Register r -> Register r
  | LabelLineNumber lbl -> Num (Float.of_int (Map.find_exn label_map lbl))
  | Num n -> Num n
  | Bool b -> Num (if b then 1. else 0.)
  | Add (a, b) -> Add (compile_expr label_map a, compile_expr label_map b)
  | Sub (a, b) -> Sub (compile_expr label_map a, compile_expr label_map b)
  | Mult (a, b) -> Mult (compile_expr label_map a, compile_expr label_map b)
  | Div (a, b) -> Div (compile_expr label_map a, compile_expr label_map b)
  | And (a, b) -> And (compile_expr label_map a, compile_expr label_map b)
  | Or (a, b) -> Or (compile_expr label_map a, compile_expr label_map b)
  | Not e -> Not (compile_expr label_map e)
  | Mod (a, b) -> Mod (compile_expr label_map a, compile_expr label_map b)
  | If_expr { conds; default } ->
      If_expr
        {
          conds =
            List.map conds ~f:(fun (cond, expr) ->
                (compile_condition label_map cond, compile_expr label_map expr));
          default = compile_expr label_map default;
        }

and compile_condition label_map : Desmos_virtual_machine.condition -> condition
    = function
  | Desmos_virtual_machine.Compare (op, a, b) ->
      Compare (op, compile_expr label_map a, compile_expr label_map b)
  | BoolVal e -> BoolVal (compile_expr label_map e)

let compile_generalized_set (reg : Register.t)
    (action : Desmos_virtual_machine.generalized_set) label_map : set list =
  let stack_reg = get_stack_register reg in
  match action with
  | Desmos_virtual_machine.Set expr -> [ (reg, compile_expr label_map expr) ]
  | PushAndSet expr ->
      [
        (stack_reg, ListJoin (Register stack_reg, Register reg));
        (reg, compile_expr label_map expr);
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

let compile_stmt label_map idx stmt =
  let pc_eq_i =
    Compare (Compare_op.Eq, Register program_counter_reg, Num (Float.of_int idx))
  in
  match stmt with
  | Desmos_virtual_machine.Exit ->
      (* TODO brady: find a better way to signal exits than setting pc to -1 *)
      [ (pc_eq_i, [ (program_counter_reg, Num (-1.)) ]) ]
  | Label _ -> []
  | Instruction sets ->
      let reg_sets =
        List.concat_map sets ~f:(fun (reg, action) ->
            compile_generalized_set reg action label_map)
      in
      let has_explicit_pc =
        List.exists sets ~f:(fun (reg, _) ->
            Register.equal reg program_counter_reg)
      in
      let all_sets = if has_explicit_pc then reg_sets else reg_sets in
      [ (pc_eq_i, all_sets) ]

let compile (program : Register.Set.t Desmos_virtual_machine.t) : t =
  let label_map = build_label_map program.main in
  let _, conds =
    List.fold_left program.main ~init:(0, []) ~f:(fun (idx, acc) stmt ->
        match stmt with
        | Desmos_virtual_machine.Label _ -> (idx, acc)
        | _ ->
            let new_conds = compile_stmt label_map idx stmt in
            (idx + 1, acc @ new_conds))
  in
  let program_action =
    (* TODO brady: should this match what Exit does? *)
    { conds; default = [ (program_counter_reg, Register program_counter_reg) ] }
  in
  let init_registers =
    let all_registers =
      Set.union program.info (Register.Set.singleton program_counter_reg)
      |> Set.to_list
    in
    List.concat_map all_registers ~f:(fun reg ->
        [
          (* TODO brady: need a way to set the initial pc value to not be 5.4321. I think the extract registers pass should actually be something like "prepare registers" and it should set up all the initial values as well. It doesn't know about the stacks unfortunately, but it should have a flag for if a register needs a stack. *)
          (reg, Num 5.4321);
          (* TODO brady: right now magic numbers will show if something goes wrong.
                   Every stack starts with an element so valid code should never be able
                   to remove it. This is also useful because our current "pop" function
                   doesn't work for lists of size 0/1. *)
          (get_stack_register reg, ListLiteral [ Num 1.2345 ]);
        ])
  in
  { program_action; init_registers }
