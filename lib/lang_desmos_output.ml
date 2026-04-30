open! Core
open Types

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

and condition = Compare_op.t * expr * expr [@@deriving sexp]

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

and latex_of_cond (op, a, b) =
  let a_latex = latex_of_expr a in
  let b_latex = latex_of_expr b in
  match op with
  | Compare_op.Eq -> a_latex ^ " = " ^ b_latex
  (* TODO brady: do != in an easy way *)
  | Ne -> failwith "TODO"
  | Gt -> a_latex ^ " > " ^ b_latex
  | Ge -> a_latex ^ " \\ge " ^ b_latex
  | Lt -> a_latex ^ " < " ^ b_latex
  | Le -> a_latex ^ " \\le " ^ b_latex

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
