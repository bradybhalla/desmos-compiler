open! Core
open Types
module Types : module type of Types

(* TODO brady: this is currently a nice way to expose the languages, but we can no longer directly define intermediate languages in tests. *)
module Languages : sig
  (** The current starting language. It is imperative and has features like
      variables, functions, while loops, if statements, and basic math
      expressions. *)
  module C_style_frontend : sig
    type 'a t [@@deriving sexp_of]

    val parse_ast : Sexp.t list -> [ `Unchecked ] t
  end

  (** An imperative language almost identical to the previous one but with
      functions stored separately from the main program. It also doesn't allow
      function calls to be inside of expressions. *)
  module C_style_separated_functions : sig
    type 'a t [@@deriving sexp_of]
  end

  (** An imperative language similar to the previous one but where all registers
      share a global scope. *)
  module C_style_registers : sig
    type t [@@deriving sexp_of]
  end

  (** A register-based instruction set that has support for functions. The
      different things you can do are:
      - Set a register to an expression
      - Call a function and possibly put the output in the given register
      - Jump to the label of the first true condition, otherwise to the default
        label
      - Return an expression from a function This language makes a distinction
        between normal statements (Set/Call) and control flow operations
        (Jump/Return). Control flow is only allowed (and required) at the end of
        a block. *)
  module Register_func_instrs : sig
    type t [@@deriving sexp_of]
  end

  (** A register-based instruction set like the previous language, but doesn't
      have direct support for functions anymore. Each register has a stack that
      its value can be pushed/popped from. The things you can do are
      - GeneralizedSet a list of (register,expression) pairs. All expressions
        are computed before any register is set, and there is an option to
        push/pop register values.
      - JumpLink will set the link register to the next line and jump to the
        label. This is the only control flow that can go in the middle of a
        block, which is allowed because the program will eventually return here.
      - Return will set the program counter to the link register value. *)
  module Register_stack_instrs : sig
    type t [@@deriving sexp_of]
  end

  (** The target language of the compiler which can be almost directly
      translated to Desmos after resolving labels. The instructions just
      set/pop/push registers and exit the program when it is complete. *)
  module Desmos_virtual_machine : sig
    type t [@@deriving sexp_of]
  end

  (** Desmos expressions that will simulate the program when the main action is
      repeatedly run. This language prints out JavaScript code that will insert
      all necessary expressions in Desmos when pasted into the console. *)
  module Desmos_output : sig
    type 'a t [@@deriving sexp_of]

    val to_pastable_javascript : [ `Sanitized ] t -> string
  end
end

module Passes : sig
  open Languages

  (* TODO plotting: also check plotting statements (not variables but everything else)
     - no calls inside them
     - maybe could even combine this pass with the next one if there is enough repeated logic
   *)
  val check_function_defs :
    [ `Unchecked ] C_style_frontend.t ->
    [ `Checked_function_defs ] C_style_frontend.t Or_error.t
  (** Error checking pass that ensures
      - function defs only in global scope
      - no duplicate function or param names
      - all branches of a function return a value *)

  (* TODO plotting: also extract plotting statements *)
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

  (* TODO plotting: might want to rething the previous pass and this one. maybe it would be easier to extract all registers and scopes (also checking them), and then go rename them. so the previous pass can compile to C_style_registers Or_error.t and the [`checked/`unchecked] should apply to C_style_registers instead  *)
  val rename_local_variables :
    [ `Checked_variable_scopes ] C_style_separated_functions.t ->
    C_style_registers.t
  (** Rename variables so each variable can be treated as a global register.
      Local variables from functions and inner scopes need to be renamed. *)

  (* TODO plotting: after resolving previous todo, check variable scopes in plotting functions. points should just be global variables. parametric should be global vars plus the given parameter.  *)

  val explicate_control : C_style_registers.t -> Register_func_instrs.t
  (** Turn control flow (if/while) into jumps. This pass also changes the
      overall layout of the program from a list of statements to a list of
      blocks which each have a label, list of statements, and control flow
      operation for the end of the block. *)

  val convert_functions_to_stack :
    Register_func_instrs.t -> Register_stack_instrs.t
  (** Compile functions and replace call stacks with local per-register stacks.
  *)

  (* TODO compression: add the optimization pass here. to compress I think it only needs to fold over the statements in each block. HOWEVER, it will miss some possible optimization for function calls unless you change how JumpLink is handled  *)

  val make_program_counter_explicit :
    Register_stack_instrs.t -> Desmos_virtual_machine.t
  (** Treat the program counter as a normal register which needs to be manually
      updated at every step. Sets initial values for all registers to 1.0 except
      the program counter which is set to 0. *)

  val generate_desmos_output :
    Desmos_virtual_machine.t -> [ `Unsanitized ] Desmos_output.t
  (** Turn the virtual machine language into a representation of actual Desmos
      expressions. *)
  (* TODO plotting: generate plotting statements desmos. parametric ones should rename the parameter with t. *)

  val sanitize_register_names :
    [ `Unsanitized ] Desmos_output.t -> [ `Sanitized ] Desmos_output.t
  (** Rename registers so they have valid Desmos names *)
  (* TODO plotting: expand to plotting statements *)
end

(** Similar to the `Passes` module but each function starts with the frontend
    language. This helps remove a lot of repetitive code in the executable and
    tests. *)
module Cumulative_passes : sig
  open Languages

  val check_function_defs :
    [ `Unchecked ] C_style_frontend.t ->
    [ `Checked_function_defs ] C_style_frontend.t Or_error.t

  val extract_function_calls_and_defs :
    [ `Unchecked ] C_style_frontend.t ->
    [ `Unchecked ] C_style_separated_functions.t Or_error.t

  val check_variables_scopes :
    [ `Unchecked ] C_style_frontend.t ->
    [ `Checked_variable_scopes ] C_style_separated_functions.t Or_error.t

  val rename_local_variables :
    [ `Unchecked ] C_style_frontend.t -> C_style_registers.t Or_error.t

  val explicate_control :
    [ `Unchecked ] C_style_frontend.t -> Register_func_instrs.t Or_error.t

  val convert_functions_to_stack :
    [ `Unchecked ] C_style_frontend.t -> Register_stack_instrs.t Or_error.t

  val make_program_counter_explicit :
    [ `Unchecked ] C_style_frontend.t -> Desmos_virtual_machine.t Or_error.t

  val generate_desmos_output :
    [ `Unchecked ] C_style_frontend.t ->
    [ `Unsanitized ] Desmos_output.t Or_error.t

  val sanitize_register_names :
    [ `Unchecked ] C_style_frontend.t ->
    [ `Sanitized ] Desmos_output.t Or_error.t
end

(** Emulator that runs the program once it has been compiled to
    `Desmos_virtual_machine.t`. This is used to make sure programs run correctly
    without needing to actually put it in Desmos. *)
module Desmos_vm_emulator : sig
  type t

  val create : Languages.Desmos_virtual_machine.t -> t
  val step : t -> [ `Done | `Not_done ]
  val run_until_done : Languages.Desmos_virtual_machine.t -> t
  val inspect_register : t -> Register.t -> float
end
