open! Core

module Register_func_instrs = struct
  type register = string [@@deriving sexp]
  type label = string [@@deriving sexp]
  type function_ = string [@@deriving sexp]

  type expr =
    | Register of register
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
    | Set of register * expr
    | Jump of { conditional : (expr * label) list; default : label }
    | Call of { func_name : function_; args : expr list; ret : register option }
  [@@deriving sexp]

  type function_def = {
    name : function_;
    params : register list;
    body : stmt list;
  }

  type 'a t = { info : 'a; functions : function_def list; main : stmt list }
end

module Register_stack_instrs = struct end
module Register_stack_multi_instrs = struct end
module Desmos_output = struct end
