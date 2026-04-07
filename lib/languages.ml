open! Core
open Types

(** Register-based language that allows for function definitions. Don't need to
    worry about saving registers when calling/returning from functions. *)
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

  type function_def = {
    name : Function_name.t;
    params : Register.t list;
    body : stmt list;
  }
  [@@deriving sexp]

  type t = { functions : function_def list; main : stmt list } [@@deriving sexp]
end

(** Register-based language that has a per-register stack (instead of
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
  [@@deriving sexp]
  (* TODO brady: right now this is the same as Register_func_instrs.expr, but it might be good to be able to refer to elements on the stack at some point to allow for more optimizations *)

  type stmt =
    | Set of Register.t * expr
    | Jump of { conds : (expr * Label.t) list; default : Label.t }
    | Push of Register.t
    | Pop of Register.t
    | Link_push of Label.t  (** call to Link_pop_jump will go to the label *)
    | Link_pop_jump
  [@@deriving sexp]

  type t = { main : stmt list } [@@deriving sexp]
end

module Register_stack_multi_instrs = struct end
(** Register-based language that allows for calling any number of statements at
    a time. At any given time a register can be modified at most once, and reads
    of the register will be from before the modification. The program counter
    update is explicit. *)

module Desmos_output = struct end
(** The output to desmos, including the runtime environment necessary to execute
    the program. *)
