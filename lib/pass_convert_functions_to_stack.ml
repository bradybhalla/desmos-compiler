open! Core
open! Languages
open! Types
module Rset = Register.Set

let rec uncover_register_expr = function
  | Register_func_instrs.Register r -> Rset.singleton r
  | Num _ | Bool _ -> Rset.empty
  | Add (a, b) | Sub (a, b) | Mult (a, b) | Div (a, b) | And (a, b) | Or (a, b)
    ->
      Set.union (uncover_register_expr a) (uncover_register_expr b)
  | Not e -> uncover_register_expr e

(* TODO brady: move the uncover functions to another pass that just pulls out all registers (and maybe function deps or other things we care about for this pass). It should also have error checking that we don't use a register before defining it. *)
let uncover_registers_stmt = function
  | Register_func_instrs.Set (reg, expr) ->
      Set.union (Rset.singleton reg) (uncover_register_expr expr)
  | Jump { conds; default = _ } ->
      conds |> List.map ~f:fst
      |> List.map ~f:uncover_register_expr
      |> List.fold ~f:Set.union ~init:Rset.empty
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
  | Return expr -> uncover_register_expr expr
  | Label _ -> Rset.empty

let uncover_registers_func (def : Register_func_instrs.function_def) =
  let params_regs = Rset.of_list def.params in
  let body_regs =
    def.body
    |> List.map ~f:uncover_registers_stmt
    |> List.fold ~f:Set.union ~init:Rset.empty
  in
  Set.union params_regs body_regs

let rec compile_expr = function
  | Register_func_instrs.Register r -> Register_stack_instrs.Register r
  | Num n -> Num n
  | Bool b -> Bool b
  | Add (a, b) -> Add (compile_expr a, compile_expr b)
  | Sub (a, b) -> Sub (compile_expr a, compile_expr b)
  | Mult (a, b) -> Mult (compile_expr a, compile_expr b)
  | Div (a, b) -> Div (compile_expr a, compile_expr b)
  | And (a, b) -> And (compile_expr a, compile_expr b)
  | Or (a, b) -> Or (compile_expr a, compile_expr b)
  | Not e -> Not (compile_expr e)

let get_function_label name =
  let function_str = name |> Function_name.to_string in
  Label.of_string ("function_entrypoint_" ^ function_str)

(* TODO brady: right now the live registers are just computed at the function level. A better way would probably be to analyze all the possible registers that could be impacted by a statement and just push the intersection of those with the function registers. I think I will eventually add a pass before this that injects liveness information. *)
let compile_stmt ~live_registers
    ~(functions : Register_func_instrs.function_def Function_name.Map.t) =
  function
  | Register_func_instrs.Set (reg, expr) ->
      [
        Register_stack_instrs.GeneralizedSet [ (reg, Set (compile_expr expr)) ];
      ]
  | Jump { conds; default } ->
      [
        Jump
          {
            conds =
              List.map conds ~f:(fun (cond, label) ->
                  (compile_expr cond, label));
            default;
          };
      ]
  | Label l -> [ Label l ]
  | Call call_info ->
      let open Register_stack_instrs in
      (* TODO brady: get liveness from prev language *)
      let need_to_save_registers = Rset.of_list live_registers in
      let arg_registers =
        let func_info = Map.find_exn functions call_info.func_name in
        func_info.params
      in
      let arg_registers_and_values =
        call_info.args |> List.map ~f:compile_expr |> List.zip_exn arg_registers
      in
      let save_registers_and_set_args =
        let set_args =
          List.map
            ~f:(fun (reg, expr) ->
              if Set.mem need_to_save_registers reg then
                (* argument that we also need to save to stack *)
                (reg, PushAndSet expr)
              else
                (* argument that we don't need to save *)
                (reg, Set expr))
            arg_registers_and_values
        in
        (* handle remaining registers that we need to save *)
        let save_remaining_clobbered =
          Set.diff need_to_save_registers (Rset.of_list arg_registers)
          |> Set.to_list
          |> List.map ~f:(fun r -> (r, Push))
        in
        GeneralizedSet (set_args @ save_remaining_clobbered)
      in
      let call_function =
        Link_push_jump (get_function_label call_info.func_name)
      in
      let restore_registers =
        GeneralizedSet
          (need_to_save_registers |> Set.to_list
          |> List.map ~f:(fun r -> (r, Pop)))
      in
      let store_result =
        call_info.ret
        |> Option.map ~f:(fun reg ->
               GeneralizedSet [ (reg, Set (Register return_register)) ])
        |> Option.to_list
      in
      [ save_registers_and_set_args; call_function; restore_registers ]
      @ store_result
  | Return expr ->
      (* put return value in the correct register, then Link_pop_jump *)
      [
        GeneralizedSet
          [ (Register_stack_instrs.return_register, Set (compile_expr expr)) ];
        Link_pop_jump;
      ]

let compile_function_def ~functions (name, def) =
  let live_registers = uncover_registers_func def |> Set.to_list in
  let entrypoint_label =
    Register_stack_instrs.Label (get_function_label name)
  in
  entrypoint_label
  :: List.concat_map ~f:(compile_stmt ~live_registers ~functions) def.body

let compile_main ~functions ~main =
  let live_registers =
    main |> List.map ~f:uncover_registers_stmt |> Rset.union_list |> Set.to_list
  in
  List.concat_map ~f:(compile_stmt ~live_registers ~functions) main

let compile { Register_func_instrs.functions; main } =
  (* need to put main first because the program starts at the beginning *)
  compile_main ~functions ~main
  @ (functions |> Map.to_alist
    |> List.concat_map ~f:(compile_function_def ~functions))
