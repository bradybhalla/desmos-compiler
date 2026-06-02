open! Core
open Languages
open Types
open Desmos_output

let build_rename_map all_registers =
  let clean, dirty =
    List.partition_tf all_registers ~f:(fun reg ->
        not (String.mem (Register.to_string reg) '_'))
  in
  let taken = String.Hash_set.of_list (List.map clean ~f:Register.to_string) in
  List.fold dirty ~init:Register.Map.empty ~f:(fun acc reg ->
      let base =
        Register.to_string reg |> String.filter ~f:(fun c -> Char.(c <> '_'))
      in
      let rec find_unique candidate =
        if Hash_set.mem taken candidate then find_unique (candidate ^ "0")
        else candidate
      in
      let new_name = find_unique base in
      Hash_set.add taken new_name;
      Map.set acc ~key:reg ~data:(Register.of_string new_name))

let rename_reg rename_map reg =
  Map.find rename_map reg |> Option.value ~default:reg

let rec rename_expr rename_map = function
  | Register r -> Register (rename_reg rename_map r)
  | Num _ as e -> e
  | Add (a, b) -> Add (rename_expr rename_map a, rename_expr rename_map b)
  | Sub (a, b) -> Sub (rename_expr rename_map a, rename_expr rename_map b)
  | Mult (a, b) -> Mult (rename_expr rename_map a, rename_expr rename_map b)
  | Div (a, b) -> Div (rename_expr rename_map a, rename_expr rename_map b)
  | And (a, b) -> And (rename_expr rename_map a, rename_expr rename_map b)
  | Or (a, b) -> Or (rename_expr rename_map a, rename_expr rename_map b)
  | Not e -> Not (rename_expr rename_map e)
  | Mod (a, b) -> Mod (rename_expr rename_map a, rename_expr rename_map b)
  | ListJoin (a, b) ->
      ListJoin (rename_expr rename_map a, rename_expr rename_map b)
  | ListSlice (a, b, c) ->
      ListSlice
        ( rename_expr rename_map a,
          rename_expr rename_map b,
          rename_expr rename_map c )
  | ListLength e -> ListLength (rename_expr rename_map e)
  | ListIndex (a, b) ->
      ListIndex (rename_expr rename_map a, rename_expr rename_map b)
  | ListLiteral exprs ->
      ListLiteral (List.map exprs ~f:(rename_expr rename_map))
  | If_expr { conds; default } ->
      If_expr
        {
          conds =
            List.map conds ~f:(fun ((op, a, b), e) ->
                ( (op, rename_expr rename_map a, rename_expr rename_map b),
                  rename_expr rename_map e ));
          default = rename_expr rename_map default;
        }

let rename_set rename_map (reg, expr) =
  (rename_reg rename_map reg, rename_expr rename_map expr)

let rename_decl rename_map (d : desmos_decl) =
  { d with reg = rename_reg rename_map d.reg }

let rename_desmos_plot rename_map = function
  | Point { x; y; args } ->
      Point { x = rename_expr rename_map x; y = rename_expr rename_map y; args }
  | Line { x1; y1; x2; y2; args } ->
      Line
        {
          x1 = rename_expr rename_map x1;
          y1 = rename_expr rename_map y1;
          x2 = rename_expr rename_map x2;
          y2 = rename_expr rename_map y2;
          args;
        }

let compile
    ({
       program_action = { conds; default };
       init_registers = { external_; internal };
       stack_inits;
       desmos_plots;
       status = `Unsanitized;
     } :
      [ `Unsanitized ] t) : [ `Sanitized ] t =
  let all_registers =
    List.map external_ ~f:(fun d -> d.reg)
    @ List.map internal ~f:(fun d -> d.reg)
    @ List.map stack_inits ~f:fst
  in
  let rename_map = build_rename_map all_registers in
  {
    program_action =
      {
        conds =
          List.map conds ~f:(fun ((op, a, b), sets) ->
              ( (op, rename_expr rename_map a, rename_expr rename_map b),
                List.map sets ~f:(rename_set rename_map) ));
        default = List.map default ~f:(rename_set rename_map);
      };
    init_registers =
      {
        external_ = List.map external_ ~f:(rename_decl rename_map);
        internal = List.map internal ~f:(rename_decl rename_map);
      };
    stack_inits = List.map stack_inits ~f:(rename_set rename_map);
    desmos_plots = List.map desmos_plots ~f:(rename_desmos_plot rename_map);
    status = `Sanitized;
  }
