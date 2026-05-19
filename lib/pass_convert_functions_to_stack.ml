open! Core
open Languages
open Types
module Rset = Register.Set

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
  | Mod (a, b) -> Mod (compile_expr a, compile_expr b)
  | Compare (op, a, b) -> Compare (op, compile_expr a, compile_expr b)
  | If_expr { conds; default } ->
      Register_stack_instrs.If_expr
        {
          conds =
            List.map conds ~f:(fun (cond, e) ->
                (compile_expr cond, compile_expr e));
          default = compile_expr default;
        }

let compile_control_flow = function
  | Register_func_instrs.Jump { conds; default } ->
      Register_stack_instrs.Jump
        {
          conds =
            List.map conds ~f:(fun (cond, lbl) -> (compile_expr cond, lbl));
          default;
        }
  | Return expr -> Return (compile_expr expr)
  | Exit -> Exit

let compile_stmt
    ~(functions : Register_func_instrs.function_def Function_name.Map.t)
    ~current_function =
  let open Register_stack_instrs in
  function
  | Register_func_instrs.Set (reg, expr) ->
      [ GeneralizedSet [ (reg, Set (compile_expr expr)) ] ]
  | Call { func_name; args; ret } ->
      (* we need to save local registers if we are currently inside a function because we might end up in a recursive call. Functions have disjoint sets of registers so this is the only case where it matters. *)
      (* TODO brady: we could make this more efficient by checking if we actually have the potential for a recursive call *)
      let need_to_save_registers =
        Option.value_map current_function ~default:Rset.empty ~f:(fun func ->
            (Map.find_exn functions func).local_registers)
      in
      let func_info = Map.find_exn functions func_name in
      (* registers used to pass in arguments *)
      let arg_registers = func_info.params in
      let arg_registers_and_values =
        args |> List.map ~f:compile_expr |> List.zip_exn arg_registers
      in
      let save_registers_and_set_args =
        (* set argument registers and possible save them to stack if they are live *)
        let set_args =
          List.map
            ~f:(fun (reg, expr) ->
              if Set.mem need_to_save_registers reg then (reg, PushAndSet expr)
              else (reg, Set expr))
            arg_registers_and_values
        in
        let save_remaining_clobbered =
          (* save other live registers that aren't used for arguments *)
          Set.diff need_to_save_registers (Rset.of_list arg_registers)
          |> Set.to_list
          |> List.map ~f:(fun r -> (r, Push))
        in
        let save_link_register = (link_register, Push) in
        GeneralizedSet
          ([ save_link_register ] @ set_args @ save_remaining_clobbered)
      in
      let call_function = JumpLink func_info.entry_label in
      let restore_registers =
        let restore_saved_registers =
          need_to_save_registers |> Set.to_list
          |> List.map ~f:(fun r -> (r, Pop))
        in
        let restore_link_register = (link_register, Pop) in
        GeneralizedSet (restore_link_register :: restore_saved_registers)
      in
      let store_result =
        ret
        |> Option.map ~f:(fun reg ->
               GeneralizedSet [ (reg, Set (Register return_register)) ])
        |> Option.to_list
      in
      [ save_registers_and_set_args; call_function; restore_registers ]
      @ store_result

let compile_function_def ~functions (func_name, def) =
  let open Register_func_instrs in
  List.map def.blocks ~f:(fun block ->
      {
        Register_stack_instrs.label = block.label;
        body =
          List.concat_map block.body
            ~f:(compile_stmt ~functions ~current_function:(Some func_name));
        control_flow = compile_control_flow block.control_flow;
      })

let compile { Register_func_instrs.functions; main; global_registers } =
  let main_blocks =
    List.map main ~f:(fun block ->
        {
          Register_stack_instrs.label = block.label;
          body =
            List.concat_map block.body
              ~f:(compile_stmt ~functions ~current_function:None);
          control_flow = compile_control_flow block.control_flow;
        })
  in
  let function_blocks =
    Map.to_alist functions
    |> List.concat_map ~f:(fun (name, def) ->
           compile_function_def ~functions (name, def))
  in
  let combined_registers =
    Map.fold functions ~init:global_registers ~f:(fun ~key:_ ~data:def acc ->
        if not (Set.is_empty (Set.inter acc def.local_registers)) then
          failwith
            "expected register sets to be disjoint. there is probably an issue \
             with register renaming.";
        Set.union acc def.local_registers)
    |> Set.union (Rset.of_list [ link_register; return_register ])
  in
  {
    Register_stack_instrs.blocks = main_blocks @ function_blocks;
    registers = combined_registers;
  }
