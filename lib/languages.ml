open! Core
open Types

let link_register = Register.of_string "00link"
let program_counter_reg = Register.of_string "00pc"
let return_register = Register.of_string "00ret"

module C_style_frontend = struct
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
    | If_expr of { conds : (expr * expr) list; default : expr }
  [@@deriving sexp]

  type stmt =
    | Function_def of unit
    | If of expr * stmt list * stmt list
    | Set of Register.t * expr
    | While of expr * stmt list
  [@@deriving sexp]
end

(*
(def f (a b c)
  (if (a > b) (
    (set x 1)
    (set y 2)
    (set z 12)
  ) else (
    (set x 1)
    (set y 1)
  ))


)

 *)

(* if the conditions are pure then you can do can put them all in a list. if they have function calls in them then they need to be nested  *)
module C_style_parsed_conditionals = struct end

(* similar to previous but has functions separated and doesn't allow for calls inside of complex statements *)
module C_style_extracted_functions = struct end

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
    | If_expr of { conds : (condition * expr) list; default : expr }
  [@@deriving sexp]

  and condition = Compare of Compare_op.t * expr * expr | BoolVal of expr
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
    | Jump of { conds : (condition * Label.t) list; default : Label.t }
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
  }
  [@@deriving sexp]

  type t = { functions : function_def Function_name.Map.t; main : block list }
  [@@deriving sexp]
end

module Register_func_instrs_with_call_liveness = struct
  (* TODO brady: maybe we can change this to use the same parameterized type as
     the previous language? it is kind of annoying to have a whole separate
     language that doesn't really change anything except a single record field
     *)
  type expr = Register_func_instrs.expr [@@deriving sexp]
  type control_flow = Register_func_instrs.control_flow [@@deriving sexp]

  type stmt =
    | Set of Register.t * expr
    | Call of {
        func_name : Function_name.t;
        args : expr list;
        ret : Register.t option;
        live_registers : Register.t list;
      }
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
  }
  [@@deriving sexp]

  type t = { functions : function_def Function_name.Map.t; main : block list }
  [@@deriving sexp]
end

(** Register-based instruction set that has a per-register stack (instead of
    functions). *)
module Register_stack_instrs = struct
  (* TODO brady: maybe make this per-function? *)

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
    | If_expr of { conds : (condition * expr) list; default : expr }
  [@@deriving sexp]

  and condition = Compare of Compare_op.t * expr * expr | BoolVal of expr
  [@@deriving sexp]

  (* TODO brady: pushandset is unneeded, we can just have a push and a set *)
  type generalized_set_action = Set of expr | PushAndSet of expr | Push | Pop
  [@@deriving sexp]

  type stmt =
    | GeneralizedSet of (Register.t * generalized_set_action) list
    (* push old values to the stack, and set new values using expression computed with the old values  *)
    (* this is a statement instead of control flow because we know we are coming back. *)
    (* TODO brady: when doing optimizations, figure out if it actually makes
       sense to have JumpLink in the middle of a block or to just have it
       with the other control flow at the end of the block *)
    | JumpLink of Label.t
  [@@deriving sexp]

  type control_flow =
    | Jump of { conds : (condition * Label.t) list; default : Label.t }
    | Return of expr
    | Exit
  [@@deriving sexp]

  type block = {
    label : Label.t;
    body : stmt list;
    control_flow : control_flow;
  }
  [@@deriving sexp]

  type t = block list [@@deriving sexp]
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
    | If_expr of { conds : (condition * expr) list; default : expr }
  [@@deriving sexp]

  and condition = Compare of Compare_op.t * expr * expr | BoolVal of expr
  [@@deriving sexp]

  type generalized_set_action = Set of expr | PushAndSet of expr | Push | Pop
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
  type 'a t = { main : block list; info : 'a } [@@deriving sexp]
end

(** The output to desmos, including the runtime environment necessary to execute
    the program. *)
module Desmos_output = struct
  let program_counter_reg = Register.of_string "00pc"

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

  and condition = Compare of Compare_op.t * expr * expr | BoolVal of expr
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
    (* TODO brady: get rid of bad symbols (just underscore?) I don't
       think we should allow spaces or other weird things in the register
       names. Other than underscore it should be the job of the frontend. *)
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
    | ListIndex (lst, i) ->
        latex_of_expr lst ^ latex_wrap_lr "[" "]" (latex_of_expr i)
    | ListLength lst -> latex_unary_operator "length" lst
    | ListLiteral exprs ->
        List.map exprs ~f:latex_of_expr
        |> String.concat ~sep:", " |> latex_wrap_lr "[" "]"
    | If_expr { conds; default } -> (
        match conds with
        | [] -> latex_of_expr default
        | conds ->
            let conds_latex =
              List.map conds ~f:(fun (cond, value) ->
                  latex_of_cond cond ^ ": " ^ latex_of_expr value)
            in
            String.concat ~sep:", " conds_latex ^ ", " ^ latex_of_expr default
            |> latex_wrap_lr "\\{" "\\}")

  and latex_of_cond = function
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

  let latex_of_set (reg, expr) =
    latex_of_register reg ^ "\\to " ^ latex_of_expr expr

  let latex_of_set_list sets =
    match sets with
    | [] -> failwith "should not have empty set list"
    | [ set ] -> latex_of_set set
    | sets ->
        sets |> List.map ~f:latex_of_set |> String.concat ~sep:", "
        |> latex_wrap_lr "(" ")"

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
          "{latex: \"" ^ escaped ^ "\"}")
    in
    "Calc.setExpressions([" ^ String.concat ~sep:", " json_equations ^ "])"
end
