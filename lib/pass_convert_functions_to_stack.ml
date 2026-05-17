open! Core
open! Languages
open! Types
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
    ~(functions : Register_func_instrs.function_def Function_name.Map.t) =
  let open Register_stack_instrs in
  function
  | Register_func_instrs.Set (reg, expr) ->
      [ GeneralizedSet [ (reg, Set (compile_expr expr)) ] ]
  | Call { func_name; args; ret; live_registers } ->
      let need_to_save_registers = Rset.of_list live_registers in
      let func_info = Map.find_exn functions func_name in
      let arg_registers = func_info.params in
      let arg_registers_and_values =
        args |> List.map ~f:compile_expr |> List.zip_exn arg_registers
      in
      let save_registers_and_set_args =
        let set_args =
          List.map
            ~f:(fun (reg, expr) ->
              if Set.mem need_to_save_registers reg then (reg, PushAndSet expr)
              else (reg, Set expr))
            arg_registers_and_values
        in
        let save_remaining_clobbered =
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

let compile_function_def ~functions ((_ : Function_name.t), def) =
  let open Register_func_instrs_with_call_liveness in
  List.map def.blocks ~f:(fun block ->
      {
        Register_stack_instrs.label = block.label;
        body = List.concat_map block.body ~f:(compile_stmt ~functions);
        control_flow = compile_control_flow block.control_flow;
      })

(* TODO important: finish propagating registers down *)
let compile
    { Register_func_instrs_with_call_liveness.functions; main; registers = _ } =
  let main_blocks =
    List.map main ~f:(fun block ->
        {
          Register_stack_instrs.label = block.label;
          body = List.concat_map block.body ~f:(compile_stmt ~functions);
          control_flow = compile_control_flow block.control_flow;
        })
  in
  let function_blocks =
    Map.to_alist functions
    |> List.concat_map ~f:(fun (name, def) ->
           compile_function_def ~functions (name, def))
  in
  main_blocks @ function_blocks
