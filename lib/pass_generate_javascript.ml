open! Core
open Languages
open Desmos_output

let latex_of_register reg =
  let reg_name = Types.Register.to_string reg in
  [%string "R_{%{reg_name}}"]

let latex_wrap_lr left right latex = "\\left" ^ left ^ latex ^ "\\right" ^ right
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
  | Add (a, b) -> latex_of_expr a ^ " + " ^ latex_of_expr b |> latex_wrap_paren
  | Sub (a, b) -> latex_of_expr a ^ " - " ^ latex_of_expr b |> latex_wrap_paren
  | Mult (a, b) ->
      latex_of_expr a ^ " \\cdot " ^ latex_of_expr b |> latex_wrap_paren
  | Div (a, b) -> latex_of_expr a ^ " / " ^ latex_of_expr b |> latex_wrap_paren
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
  | Types.Compare_op.Eq -> a_latex ^ " = " ^ b_latex
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

let decl_to_set (d : desmos_decl) : set = (d.reg, Num d.init)

(* TODO plotting: should make an internal representation of the desmos expression type and generate those, right now this is much more error prone and won't let me add args so lines will be wrong.  *)
let latex_of_plot =
  let latex_of_point x y =
    latex_of_expr x ^ ", " ^ latex_of_expr y |> latex_wrap_paren
  in
  function
  | Point { x; y; args = () } -> latex_of_point x y
  | Line { x1; y1; x2; y2; args = () } ->
      latex_of_point x1 y1 ^ ", " ^ latex_of_point x2 y2
      |> latex_wrap_lr "[" "]"

let compile
    ({
       program_action;
       init_registers = { external_; internal };
       stack_inits;
       desmos_plots;
       status = `Sanitized;
     } :
      [ `Sanitized ] t) : Javascript_setup.t =
  let program_desmos_line = "M_{ain} = " ^ latex_of_action program_action in
  (* don't reset external vars *)
  let reset_desmos_line =
    "R_{eset} = "
    ^ latex_of_action
        { conds = []; default = List.map internal ~f:decl_to_set @ stack_inits }
  in
  let external_desmos_lines =
    List.map external_ ~f:(fun d ->
        latex_of_register d.reg ^ "=" ^ latex_of_expr (Num d.init))
  in
  let plots = List.map desmos_plots ~f:latex_of_plot in
  let internal_desmos_lines =
    List.map internal ~f:(fun d ->
        latex_of_register d.reg ^ "=" ^ latex_of_expr (Num d.init))
  in
  let stack_desmos_lines =
    List.map stack_inits ~f:(fun (reg, expr) ->
        latex_of_register reg ^ "=" ^ latex_of_expr expr)
  in
  let all_lines =
    [ program_desmos_line; reset_desmos_line ]
    @ external_desmos_lines @ plots @ internal_desmos_lines @ stack_desmos_lines
  in
  let json_equations =
    List.map all_lines ~f:(fun str ->
        let escaped =
          String.substr_replace_all ~pattern:"\\" ~with_:"\\\\" str
        in
        "{latex: \"" ^ escaped ^ "\"}")
  in
  "Calc.setExpressions([" ^ String.concat ~sep:", " json_equations ^ "])"
