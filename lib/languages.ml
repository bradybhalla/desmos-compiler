open! Core
open Types

let link_register = Register.of_string "00link"
let program_counter_reg = Register.of_string "00pc"
let return_register = Register.of_string "00ret"

(* TODO brady: after lists exist, it would be nice to have a "for" construct
   over a range.

   (for i (1..5)
      (set j (/ i 2))
      (update j))

   *)

module C_style_frontend = Lang_c_style_frontend

(* if the conditions are pure then you can do can put them all in a list. if they have function calls in them then they need to be nested  *)
module C_style_separated_functions = struct
  type expr =
    | Unit
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
    | Mod of expr * expr
    | Compare of Compare_op.t * expr * expr
    | If_expr of { conds : (expr * expr) list; default : expr }
  [@@deriving sexp]

  type stmt =
    | Return of expr
    | If of { branches : (expr * stmt list) list; else_ : stmt list }
    | Set of Register.t * expr
    | Decl of Register.t
    | While of { cond : expr; body : stmt list }
    | Call of {
        func_name : Function_name.t;
        args : expr list;
        ret : Register.t option;
      }
  [@@deriving sexp]

  type function_def = { params : Register.t list; body : stmt list }
  [@@deriving sexp]

  type 'a t = {
    functions : function_def Function_name.Map.t;
    main : stmt list;
    info : 'a;
  }
  [@@deriving sexp]
end

module C_style_registers = struct
  type expr = C_style_separated_functions.expr [@@deriving sexp]

  type stmt =
    | Return of expr
    | If of { branches : (expr * stmt list) list; else_ : stmt list }
    | Set of Register.t * expr
    | While of { cond : expr; body : stmt list }
    | Call of {
        func_name : Function_name.t;
        args : expr list;
        ret : Register.t option;
      }
  [@@deriving sexp]

  type function_def = {
    params : Register.t list;
    body : stmt list;
    local_registers : Register.Set.t;
  }
  [@@deriving sexp]

  type t = {
    functions : function_def Function_name.Map.t;
    main : stmt list;
    global_registers : Register.Set.t;
  }
  [@@deriving sexp]
end

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
    | Mod of expr * expr
    | Compare of Compare_op.t * expr * expr
    | If_expr of { conds : (expr * expr) list; default : expr }
  [@@deriving sexp]

  type stmt =
    | Set of Register.t * expr
    | Call of {
        func_name : Function_name.t;
        args : expr list;
        ret : Register.t option;
      }
  [@@deriving sexp]

  type control_flow =
    | Jump of { conds : (expr * Label.t) list; default : Label.t }
    | Return of expr
    | Exit
  [@@deriving sexp]

  type block = {
    label : Label.t;
    body : stmt list;
    control_flow : control_flow;
  }
  [@@deriving sexp]

  type function_def = {
    entry_label : Label.t;
    params : Register.t list;
    blocks : block list;
    local_registers : Register.Set.t;
  }
  [@@deriving sexp]

  type t = {
    functions : function_def Function_name.Map.t;
    main : block list;
    global_registers : Register.Set.t;
  }
  [@@deriving sexp]
end

(** Register-based instruction set that has a per-register stack (instead of
    functions). *)
module Register_stack_instrs = struct
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
    | Mod of expr * expr
    | Compare of Compare_op.t * expr * expr
    | If_expr of { conds : (expr * expr) list; default : expr }
  [@@deriving sexp]

  (* TODO brady: figure out which of the stack instrucitons are actually needed and remove others, propagate down *)
  type generalized_set_action = Set of expr | PushAndSet of expr | Push | Pop
  [@@deriving sexp]

  type stmt = GeneralizedSet of (Register.t * generalized_set_action) list
  [@@deriving sexp]

  type control_flow =
    | Jump of { conds : (expr * Label.t) list; default : Label.t }
    | JumpLink of { target : Label.t; return_label : Label.t }
    | Return of expr
    | Exit
  [@@deriving sexp]

  type block = {
    label : Label.t;
    body : stmt list;
    control_flow : control_flow;
  }
  [@@deriving sexp]

  type t = { blocks : block list; registers : Register.Set.t } [@@deriving sexp]
end

(** Register-based instruction set where each instruction is a list of register
    sets that execute simultaneously. Jumps are expressed by setting
    program_counter_reg explicitly. Labels are resolved to line numbers by the
    next pass via LabelLineNumber. *)
module Desmos_virtual_machine = struct
  type expr =
    | Register of Register.t
    | LabelLineNumber of Label.t
    | Num of float
    | Bool of bool
    | Add of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Not of expr
    | Mod of expr * expr
    | Compare of Compare_op.t * expr * expr
    | If_expr of { conds : (expr * expr) list; default : expr }
  [@@deriving sexp]

  type generalized_set_action =
    | Set of expr
    | PushAndSet of expr
    | PushExprAndSet of { push : expr; set : expr }
    | Push
    | Pop
  [@@deriving sexp]

  (* TODO brady: for optimizations it might be kind of hard to determine if
    two instructions can be combined (we need to search for pc -> pc+1).
    maybe we need another intermediate language where there are Instructions
    and ManualPCInstructions? Then we optimize there and have a small pass
    turning it into this language? *)
  (* TODO brady: this Exit should be able to be represented as PC <- -1 or something, then we don't need a whole instruction *)
  type stmt = Instruction of (Register.t * generalized_set_action) list | Exit
  [@@deriving sexp]

  type block = { label : Label.t; body : stmt list } [@@deriving sexp]

  type t = { main : block list; registers : expr Register.Map.t }
  [@@deriving sexp]
end

module Desmos_output = Lang_desmos_output
(** The output to desmos, including the runtime environment necessary to execute
    the program. *)
