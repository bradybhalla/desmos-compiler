open! Core
open Types

(** Register-based instruction set that allows for function definitions. Don't
    need to worry about saving registers when calling/returning from functions.
*)
module Register_func_instrs = struct
  type expr =
    | Register of Register.t
    | Num of float
    | Bool of bool
    | Add of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Not of expr
  [@@deriving sexp]

  type stmt =
    | Set of Register.t * expr
    | Jump of { conds : (expr * Label.t) list; default : Label.t }
    | Call of {
        func_name : Function_name.t;
        args : expr list;
        ret : Register.t option;
      }
    | Return of expr
    | Label of Label.t
  [@@deriving sexp]

  type function_def = { params : Register.t list; body : stmt list }
  [@@deriving sexp]

  type t = { functions : function_def Function_name.Map.t; main : stmt list }
  [@@deriving sexp]
end

module Register_func_instrs_with_liveness = struct end

(** Register-based instruction set that has a per-register stack (instead of
    functions). *)
module Register_stack_instrs = struct
  (* TODO brady: maybe make this per-function? *)
  let return_register = Register.of_string ".ret"

  type expr =
    | Register of Register.t
    | Num of float
    | Bool of bool
    | Add of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Not of expr
  [@@deriving sexp]

  type generalized_set_action = Set of expr | PushAndSet of expr | Push | Pop
  [@@deriving sexp]

  type stmt =
    | Jump of { conds : (expr * Label.t) list; default : Label.t }
    | Label of Label.t
    | GeneralizedSet of (Register.t * generalized_set_action) list
      (* push old values to the stack, and set new values using expression computed with the old values  *)
    | Link_push_jump of Label.t
      (* call to Link_pop_jump will go to the next line. jump to label *)
    | Link_pop_jump
  [@@deriving sexp]

  type t = stmt list [@@deriving sexp]
end

(** Register-based instruction set that allows for calling any number of
    statements at a time. At any given time a register can be modified at most
    once, and reads of the register will be from before the modification. The
    program counter update is also explicit in this instruction set. *)
module Desmos_virtual_machine = struct
  let link_register = Register.of_string ".link"

  type expr =
    | Register of Register.t
    | ProgramCounter
    | Num of float
    | Bool of bool
    | Add of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Not of expr
  [@@deriving sexp]

  type generalized_set = Set of expr | PushAndSet of expr | Push | Pop
  [@@deriving sexp]

  (* TODO brady: for better optimization might need specially handle function calls instead of allowing general registers? *)
  type jump_target = JumpToLabel of Label.t | JumpToRegister of Register.t
  [@@deriving sexp]

  type pc_action =
    | NextInstr
    | Jump of { conds : (expr * jump_target) list; default : jump_target }
    | Exit
  [@@deriving sexp]

  type instruction = (Register.t * generalized_set) list * pc_action
  [@@deriving sexp]

  (* TODO brady: maybe consider grouping by labels instead of integrating them with the code? this would need to start at an earlier language. Then each group would be defined by (stmt list, jump). Honestly this could make things a lot nicer. *)
  type stmt = Label of Label.t | Instruction of instruction [@@deriving sexp]
  type t = { main : stmt list; registers : Register.Set.t } [@@deriving sexp]
end

module Desmos_output = struct end
(** The output to desmos, including the runtime environment necessary to execute
    the program. *)
