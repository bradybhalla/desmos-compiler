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

(* TODO brady: not every register needs a stack, so we should find a better way to represent all the different register properties *)
let get_stack_register reg =
  (* TODO: right now nothing else should start with 2, but maybe there is a better way to enforce the uniqueness here *)
  Register.of_string ("2" ^ Register.to_string reg ^ "Stack")

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
  | PushExprAndSet { push; set } ->
      [
        (stack_reg, ListJoin (Register stack_reg, compile_expr label_map push));
        (reg, compile_expr label_map set);
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

let compile_desmos_plot label_map = function
  | Desmos_virtual_machine.Point { x; y; args } ->
      Desmos_output.Point
        { x = compile_expr label_map x; y = compile_expr label_map y; args }
  | Line { x1; y1; x2; y2; args } ->
      Line
        {
          x1 = compile_expr label_map x1;
          y1 = compile_expr label_map y1;
          x2 = compile_expr label_map x2;
          y2 = compile_expr label_map y2;
          args;
        }

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
  let external_regs =
    program.desmos_vars
    |> List.map ~f:(fun (d : Desmos_virtual_machine.desmos_decl) -> d.reg)
    |> Register.Set.of_list
  in
  let internal =
    Map.to_alist program.registers
    |> List.filter_map ~f:(fun (reg, init_expr) ->
           if Set.mem external_regs reg then None
           else
             let init =
               match init_expr with
               | Desmos_virtual_machine.Num n -> n
               | _ -> failwith "expected numeric init for internal register"
             in
             Some
               {
                 C_style_separated_functions.reg;
                 init;
                 args = Desmos_slider_args.default;
               })
  in
  let stack_inits =
    Map.keys program.registers
    |> List.map ~f:(fun reg ->
           (* TODO brady: right now magic numbers will show if something goes wrong.
              Every stack starts with an element so valid code should never be able
              to remove it. This is also useful because our current "pop" function
              doesn't work for lists of size 0/1. *)
           (get_stack_register reg, ListLiteral [ Num 5.4321 ]))
  in
  {
    program_action;
    init_registers = { external_ = program.desmos_vars; internal };
    stack_inits;
    desmos_plots = List.map program.desmos_plots ~f:(compile_desmos_plot label_map);
    status = `Unsanitized;
  }
