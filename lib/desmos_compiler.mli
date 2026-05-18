open! Core
open Types
module Types : module type of Types
module Languages : module type of Languages

module Passes : sig
  open Languages

  val check_function_defs :
    [ `Unchecked ] C_style_frontend.t ->
    [ `Checked_function_defs ] C_style_frontend.t Or_error.t
  (** Error checking pass that ensures
      - function defs only in global scope
      - no duplicate function or param names
      - all branches of a function return a value *)

  val extract_function_calls_and_defs :
    [ `Checked_function_defs ] C_style_frontend.t ->
    [ `Unchecked ] C_style_separated_functions.t
  (** When a function call is inside of a complex expression, move it to a
      separate statement which saves the value to a register first. An important
      part of this is correctly handling short circuiting logic. For example if
      an and/or/conditional contains a function call, it may need to be turned
      into an if statement to ensure the function is only called when it is
      necessary. *)

  val check_variables_scopes :
    [ `Unchecked ] C_style_separated_functions.t ->
    [ `Checked_variable_scopes ] C_style_separated_functions.t Or_error.t
  (** Error checking pass that ensures
      - variables are only used after being declared
      - all variables references are in scope *)

  val rename_local_variables :
    [ `Checked_variable_scopes ] C_style_separated_functions.t ->
    C_style_registers.t
  (** Rename variables so each variable can be treated as a global register.
      Local variables from functions and inner scopes need to be renamed. *)

  val explicate_control : C_style_registers.t -> Register_func_instrs.t
  (** Turn control flow (if/while) into jumps. This pass also changes the
      overall layout of the program from a list of statements to a list of
      blocks which each have a label, list of statements, and control flow
      operation for the end of the block. *)

  val convert_functions_to_stack :
    Register_func_instrs.t -> Register_stack_instrs.t
  (** Compile functions and replace call stacks with local per-register stacks.
  *)

  val make_program_counter_explicit :
    Register_stack_instrs.t -> Desmos_virtual_machine.t
  (** Treat the program counter as a normal register which needs to be manually
      updated at every step. Sets initial values for all registers to 1.0 except
      the program counter which is set to 0. *)

  val generate_desmos_output :
    Desmos_virtual_machine.t -> [ `Unsanitized ] Desmos_output.t
  (** Turn the virtual machine language into a representation of actual Desmos
      expressions. *)

  val sanitize_register_names :
    [ `Unsanitized ] Desmos_output.t -> [ `Sanitized ] Desmos_output.t
  (** Rename registers so they have valid Desmos names *)
end

module Desmos_vm_emulator : sig
  open Languages

  type t

  val create : Desmos_virtual_machine.t -> t
  val step : t -> [ `Done | `Not_done ]
  val run_until_done : Desmos_virtual_machine.t -> t
  val inspect_register : t -> Register.t -> float
end

(* TODO: figure out how to expose languages more safely *)
(* module Languages : sig *)
(*   module C_style_frontend : sig *)
(*     type 'a t [@@deriving sexp_of] *)
(**)
(*     val parse_ast : Sexp.t list -> [ `Unchecked ] t *)
(*   end *)
(**)
(*   module C_style_separated_functions : sig *)
(*     type 'a t [@@deriving sexp_of] *)
(*   end *)
(**)
(*   module C_style_registers : sig *)
(*     type t [@@deriving sexp_of] *)
(*   end *)
(**)
(*   module Register_func_instrs : sig *)
(*     type t [@@deriving sexp_of] *)
(*   end *)
(**)
(*   module Register_func_instrs_with_call_liveness : sig *)
(*     type t [@@deriving sexp_of] *)
(*   end *)
(**)
(*   module Register_stack_instrs : sig *)
(*     type t [@@deriving sexp_of] *)
(*   end *)
(**)
(*   module Desmos_virtual_machine : sig *)
(*     module Initial_registers : sig *)
(*       type t [@@deriving sexp_of] *)
(*     end *)
(**)
(*     type 'a t [@@deriving sexp_of] *)
(*   end *)
(**)
(*   module Desmos_output : sig *)
(*     type 'a t [@@deriving sexp_of] *)
(**)
(*     val to_pastable_javascript : [ `Sanitized ] t -> string *)
(*   end *)
(* end *)
