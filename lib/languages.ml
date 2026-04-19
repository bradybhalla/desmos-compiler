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
    | Mod of expr * expr
    | Compare of Compare_op.t * expr * expr
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
    | Mod of expr * expr
    | Compare of Compare_op.t * expr * expr
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
    | Exit
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
    | Mod of expr * expr
    | Compare of Compare_op.t * expr * expr
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

(** The output to desmos, including the runtime environment necessary to execute
    the program. *)
module Desmos_output = struct
  let program_counter_reg = Register.of_string "pc"

  type expr =
    | Register of Register.t
    | ProgramCounter
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
  [@@deriving sexp]

  type condition = Compare of Compare_op.t * expr * expr | BoolVal of expr
  [@@deriving sexp]

  type set = Register.t * expr [@@deriving sexp]

  type action = { conds : (condition * set list) list; default : set list }
  [@@deriving sexp]

  type t = { program_action : action; init_registers : set list }
  [@@deriving sexp]

  let get_stack_register reg =
    Register.of_string (Register.to_string reg ^ "Stack")

  let latex_of_register reg =
    let reg_name = Register.to_string reg in
    (* TODO brady: get rid of bad symbols *)
    [%string "R_{%{reg_name}}"]

  let latex_wrap_lr left right latex =
    "\\left" ^ left ^ latex ^ "\\right" ^ right

  let latex_wrap_paren = latex_wrap_lr "(" ")"

  let rec latex_of_expr =
    let latex_binary_operator name a b =
      "\\operatorname{" ^ name ^ "}"
      ^ latex_wrap_paren (latex_of_expr a ^ ", " ^ latex_of_expr b)
    in
    let latex_unary_operator name a =
      "\\operatorname{" ^ name ^ "}" ^ latex_wrap_paren (latex_of_expr a)
    in
    function
    | Register reg -> latex_of_register reg
    | ProgramCounter -> latex_of_register program_counter_reg
    | Num n ->
        let n_str = Float.to_string n in
        String.chop_suffix_if_exists n_str ~suffix:"."
    | Add (a, b) ->
        latex_of_expr a ^ " + " ^ latex_of_expr b |> latex_wrap_paren
    | Sub (a, b) ->
        latex_of_expr a ^ " - " ^ latex_of_expr b |> latex_wrap_paren
    | Mult (a, b) ->
        latex_of_expr a ^ " \\cdot " ^ latex_of_expr b |> latex_wrap_paren
    | Div (a, b) ->
        latex_of_expr a ^ " / " ^ latex_of_expr b |> latex_wrap_paren
    | And (a, b) -> latex_of_expr (Mult (a, b)) |> latex_wrap_paren
    | Or (a, b) ->
        latex_of_expr (Sub (Add (a, b), Mult (a, b))) |> latex_wrap_paren
    | Not a -> latex_of_expr (Sub (Num 1., a)) |> latex_wrap_paren
    | Mod (a, b) -> latex_binary_operator "mod" a b
    | ListJoin (a, b) -> latex_binary_operator "join" a b
    | ListSlice (lst, istart, iend) ->
        latex_of_expr lst
        ^ latex_wrap_lr "[" "]"
            (latex_of_expr istart ^ " ... " ^ latex_of_expr iend)
    | ListLength lst -> latex_unary_operator "length" lst

  let latex_of_set (reg, expr) =
    latex_of_register reg ^ "\\to " ^ latex_of_expr expr

  let latex_of_set_list sets =
    match sets with
    | [] -> failwith "should not have empty set list"
    | [ set ] -> latex_of_set set
    | sets ->
        sets |> List.map ~f:latex_of_set |> String.concat ~sep:", "
        |> latex_wrap_lr "(" ")"

  let rec latex_of_cond = function
    | Compare (op, a, b) -> (
        let a_latex = latex_of_expr a in
        let b_latex = latex_of_expr b in
        match op with
        | Eq -> a_latex ^ " = " ^ b_latex
        (* TODO brady: do != in an easy way *)
        | Ne -> failwith "TODO"
        | Gt -> a_latex ^ " > " ^ b_latex
        | Ge -> a_latex ^ " \\ge " ^ b_latex
        | Lt -> a_latex ^ " < " ^ b_latex
        | Le -> a_latex ^ " \\le " ^ b_latex)
    (* booleans are either 1 or 0 *)
    | BoolVal v -> latex_of_cond (Compare (Eq, v, Num 1.))

  let latex_of_action { conds; default } =
    match conds with
    | [] -> latex_of_set_list default
    | conds ->
        let conds_latex =
          List.map conds ~f:(fun (cond, sets) ->
              latex_of_cond cond ^ ": " ^ latex_of_set_list sets)
        in
        String.concat ~sep:", " conds_latex ^ ", " ^ latex_of_set_list default
        |> latex_wrap_lr "\\{" "\\}"

  let to_pastable_javascript { program_action; init_registers } =
    let action_latex = latex_of_action program_action in
    let register_latex =
      List.map init_registers ~f:(fun (reg, expr) ->
          latex_of_register reg ^ "=" ^ latex_of_expr expr)
    in
    let json_equations =
      List.map (action_latex :: register_latex) ~f:(fun str ->
          let escaped =
            String.substr_replace_all ~pattern:"\\" ~with_:"\\\\" str
          in
          "{latex: " ^ escaped ^ "}")
    in
    "Calc.set_Expressions([" ^ String.concat ~sep:", " json_equations ^ "])"
end
