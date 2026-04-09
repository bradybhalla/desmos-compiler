open! Core
open! Languages
open! Types
open Desmos_virtual_machine

let compile_expr = failwith "TODO"

let compile_stmt = function
  | Register_stack_instrs.GeneralizedSet set_actions -> failwith "TODO"
  | Jump { conds; default } -> failwith "TODO"
  | Label lbl -> [ Label lbl ]
  | Link_push_jump _ -> failwith "TODO"
  | Link_pop_jump -> failwith "TODO"

let compile = List.concat_map ~f:compile_stmt
