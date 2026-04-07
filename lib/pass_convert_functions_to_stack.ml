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

let uncover_registers_func
    ({ params; body; name = _ } : Register_func_instrs.function_def) =
  let params_regs = Rset.of_list params in
  let body_regs =
    body
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

let compile_stmt = function
  | Register_func_instrs.Set (reg, expr) ->
      [ Register_stack_instrs.Set (reg, compile_expr expr) ]
  | Jump { conds; default } ->
      [
        Jump
          {
            conds = List.map conds ~f:(fun (e, l) -> (compile_expr e, l));
            default;
          };
      ]
  | Label l -> [ Label l ]
  | Call _ -> failwith "TODO"
  | Return _ -> failwith "TODO"

let get_function_label name =
  let function_str = name |> Function_name.to_string in
  Label.of_string ("function_entrypoint_" ^ function_str)

let compile_function_def { Register_func_instrs.name = _; params = _; body = _ }
    =
  failwith "TODO"

let compile ({ functions; main } : Register_func_instrs.t) :
    Register_stack_instrs.t =
  let compiled_main = main |> List.map ~f:compile_stmt |> List.concat in
  let compiled_functions =
    functions |> List.map ~f:compile_function_def |> List.concat
  in
  compiled_main @ compiled_functions
