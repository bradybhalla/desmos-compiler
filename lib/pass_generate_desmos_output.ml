open! Core
open Languages
open Types
open Desmos_output

let build_label_map blocks =
  let _, label_map =
    List.fold_left blocks ~init:(0, Label.Map.empty)
      ~f:(fun (idx, m) (block : Desmos_virtual_machine.block) ->
        let m = Map.add_exn m ~key:block.label ~data:idx in
        (idx + List.length block.body, m))
  in
  label_map

let true_lit = Num 1.
let false_lit = Num 0.

let rec compile_expr label_map = function
  | Desmos_virtual_machine.Register r -> Register r
  | LabelLineNumber lbl -> Num (Float.of_int (Map.find_exn label_map lbl))
  | Num n -> Num n
  | Bool b -> if b then true_lit else false_lit
  | Add (a, b) -> Add (compile_expr label_map a, compile_expr label_map b)
  | Sub (a, b) -> Sub (compile_expr label_map a, compile_expr label_map b)
  | Mult (a, b) -> Mult (compile_expr label_map a, compile_expr label_map b)
  | Div (a, b) -> Div (compile_expr label_map a, compile_expr label_map b)
  | And (a, b) -> And (compile_expr label_map a, compile_expr label_map b)
  | Or (a, b) -> Or (compile_expr label_map a, compile_expr label_map b)
  | Not e -> Not (compile_expr label_map e)
  | Mod (a, b) -> Mod (compile_expr label_map a, compile_expr label_map b)
  | Compare (op, a, b) ->
      If_expr
        {
          conds =
            [
              ( (op, compile_expr label_map a, compile_expr label_map b),
                true_lit );
            ];
          default = false_lit;
        }
  | If_expr { conds; default } ->
      If_expr
        {
          conds =
            List.map conds ~f:(fun (cond_expr, expr) ->
                let cond =
                  match cond_expr with
                  | Desmos_virtual_machine.Compare (op, a, b) ->
                      (op, compile_expr label_map a, compile_expr label_map b)
                  | _ ->
                      (Compare_op.Eq, compile_expr label_map cond_expr, Num 1.)
                in
                (cond, compile_expr label_map expr));
          default = compile_expr label_map default;
        }

let compile_generalized_set reg action label_map : set list =
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
    (Compare_op.Eq, Register program_counter_reg, Num (Float.of_int idx))
  in
  match stmt with
  | Desmos_virtual_machine.Exit ->
      (* TODO brady: find a better way to signal exits than setting pc to -1 *)
      [ (pc_eq_i, [ (program_counter_reg, Num (-1.)) ]) ]
  | Instruction sets ->
      let reg_sets =
        List.concat_map sets ~f:(fun (reg, action) ->
            compile_generalized_set reg action label_map)
      in
      [ (pc_eq_i, reg_sets) ]

let compile (program : Desmos_virtual_machine.t) =
  let label_map = build_label_map program.main in
  let _, conds =
    List.fold_left program.main ~init:(0, []) ~f:(fun (idx, acc) block ->
        let idx, block_conds =
          List.fold_left block.body ~init:(idx, []) ~f:(fun (idx, acc) stmt ->
              let new_conds = compile_stmt label_map idx stmt in
              (idx + 1, acc @ new_conds))
        in
        (idx, acc @ block_conds))
  in
  let program_action =
    { conds; default = [ (program_counter_reg, Register program_counter_reg) ] }
  in
  let init_registers =
    Map.to_alist program.registers
    |> List.concat_map ~f:(fun (reg, init_expr) ->
           [
             (reg, compile_expr label_map init_expr);
             (* TODO brady: right now magic numbers will show if something goes wrong.
                Every stack starts with an element so valid code should never be able
                to remove it. This is also useful because our current "pop" function
                doesn't work for lists of size 0/1. *)
             (get_stack_register reg, ListLiteral [ Num 5.4321 ]);
           ])
  in
  { program_action; init_registers; info = `Unsanitized }
