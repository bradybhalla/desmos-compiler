open! Core
open Types

(* TODO brady: find a better place to put these special registers *)
let program_counter_reg = Register.of_string "00pc"
let return_register = Register.of_string "00ret"

module C_style_frontend = Lang_c_style_frontend

(* TODO brady: make binary operations a more general Binary_op of Bin_op * expr * expr so there is less boilerplate   *)

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

  type desmos_decl = {
    reg : Register.t;
    init : float;
    args : Desmos_slider_args.t;
  }
  [@@deriving sexp]

  type desmos_plot =
    | Point of { x : expr; y : expr; args : Desmos_point_args.t }
    | Line of {
        x1 : expr;
        y1 : expr;
        x2 : expr;
        y2 : expr;
        args : Desmos_line_args.t;
      }
  [@@deriving sexp]

  type function_def = { params : Register.t list; body : stmt list }
  [@@deriving sexp]

  type 'error_checking_status t = {
    functions : function_def Function_name.Map.t;
    main : stmt list;
    status : 'error_checking_status;
    desmos_decls : desmos_decl list;
    desmos_plot : desmos_plot list;
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

  type desmos_decl = C_style_separated_functions.desmos_decl [@@deriving sexp]
  type desmos_plot = C_style_separated_functions.desmos_plot [@@deriving sexp]

  type t = {
    functions : function_def Function_name.Map.t;
    main : stmt list;
    global_registers : Register.Set.t;
    desmos_decls : desmos_decl list;
    desmos_plot : desmos_plot list;
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
  type expr = Register_func_instrs.expr [@@deriving sexp]

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

  (* TODO brady: this Exit should be able to be represented as PC <- -1 or something, then we don't need a whole instruction *)
  type stmt = Instruction of (Register.t * generalized_set_action) list | Exit
  [@@deriving sexp]

  type block = { label : Label.t; body : stmt list } [@@deriving sexp]

  type t = { main : block list; registers : expr Register.Map.t }
  [@@deriving sexp]
end

(** The output to desmos, including the runtime environment necessary to execute
    the program. *)
module Desmos_output = struct
  type expr =
    | Register of Register.t
    | Num of float
    | Add of expr * expr
    | Sub of expr * expr
    | Mult of expr * expr
    | Div of expr * expr
    | And of expr * expr
    | Or of expr * expr
    | Not of expr
    | Mod of expr * expr
    | ListJoin of expr * expr
    | ListSlice of expr * expr * expr
    | ListLength of expr
    | ListIndex of expr * expr
    | If_expr of { conds : (condition * expr) list; default : expr }
    | ListLiteral of expr list
  [@@deriving sexp]

  and condition = Compare_op.t * expr * expr [@@deriving sexp]

  type set = Register.t * expr [@@deriving sexp]

  type action = { conds : (condition * set list) list; default : set list }
  [@@deriving sexp]

  type 'error_checking_status t = {
    program_action : action;
    init_registers : set list;
    status : 'error_checking_status;
  }
  [@@deriving sexp]
end

(** The javascript calls to the Desmos API needed to set up the graph so it can
    be run *)
module Javascript_setup = struct
  type t = string [@@deriving sexp]

  let to_string = Fun.id
end
