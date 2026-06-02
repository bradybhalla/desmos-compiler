open! Core
open Languages
open Types
module Rset = Register.Set
open Register_func_instrs

let label_gen = Label_generator.create "convert_funcs_to_stack"

(* TODO brady: these languages have the same expression type now, but leaving this here in case that changes in the future. If its not going to change this can be removed. *)
let compile_expr = Fun.id

let compile_control_flow = function
  | Jump { conds; default } ->
      Register_stack_instrs.Jump
        {
          conds =
            List.map conds ~f:(fun (cond, lbl) -> (compile_expr cond, lbl));
          default;
        }
  | Return expr -> Return (compile_expr expr)
  | Exit -> Exit

let compile_stmt ~(functions : function_def Function_name.Map.t)
    ~current_function (label, stmts_rev, blocks_rev) =
  let open Register_stack_instrs in
  function
  | Register_func_instrs.Set (reg, expr) ->
      ( label,
        GeneralizedSet [ (reg, Set (compile_expr expr)) ] :: stmts_rev,
        blocks_rev )
  | Call { func_name; args; ret } ->
      (* we need to save local registers if we are currently inside a function because we might end up in a recursive call. Functions have disjoint sets of registers so this is the only case where it matters. *)
      (* TODO brady: we could make this more efficient by checking if we actually have the potential for a recursive call. another optimization would be to see which local registers are actually live at this point. some might not be used anymore so we don't care what happens to them. others will be written to before being read in the future so we don't care about those either. *)
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
        GeneralizedSet (set_args @ save_remaining_clobbered)
      in
      let next_block_label = Label_generator.generate label_gen in
      let block_before_call =
        {
          Register_stack_instrs.label;
          body = List.rev (save_registers_and_set_args :: stmts_rev);
          control_flow =
            JumpLink
              {
                target = func_info.entry_label;
                return_label = next_block_label;
              };
        }
      in
      let restore_registers =
        let restore_saved_registers =
          need_to_save_registers |> Set.to_list
          |> List.map ~f:(fun r -> (r, Pop))
        in
        GeneralizedSet restore_saved_registers
      in
      let store_result =
        ret
        |> Option.map ~f:(fun reg ->
               GeneralizedSet [ (reg, Set (Register return_register)) ])
        |> Option.to_list
      in
      ( next_block_label,
        store_result @ [ restore_registers ],
        block_before_call :: blocks_rev )

let compile_block ~functions ~current_function block =
  let label, stmts_rev, additional_blocks_rev =
    List.fold_left
      ~f:(fun acc stmt -> compile_stmt ~functions ~current_function acc stmt)
      ~init:(block.label, [], []) block.body
  in
  List.rev
    ({
       Register_stack_instrs.label;
       body = List.rev stmts_rev;
       control_flow = compile_control_flow block.control_flow;
     }
    :: additional_blocks_rev)

let compile
    { Register_func_instrs.functions; main; global_registers; desmos_vars;
      desmos_plots } =
  Label_generator.reset label_gen;
  let main_blocks =
    List.concat_map main ~f:(compile_block ~functions ~current_function:None)
  in
  let function_blocks =
    Map.to_alist functions
    |> List.concat_map ~f:(fun (func_name, def) ->
           List.concat_map def.blocks
             ~f:(compile_block ~functions ~current_function:(Some func_name)))
  in
  let combined_registers =
    Map.fold functions ~init:global_registers ~f:(fun ~key:_ ~data:def acc ->
        if not (Set.is_empty (Set.inter acc def.local_registers)) then
          failwith
            "expected register sets to be disjoint. there is probably an issue \
             with register renaming.";
        Set.union acc def.local_registers)
    |> Set.union (Rset.singleton return_register)
  in
  {
    Register_stack_instrs.blocks = main_blocks @ function_blocks;
    registers = combined_registers;
    desmos_vars;
    desmos_plots;
  }
