open! Core
open Languages
open Types

(* make sure vars are only used after being declared and that all varaibles are in scope.  *)

let rename_in_expr ~renamed e =
  let rec helper = function
    | (C_style_separated_functions.Unit | Num _ | Bool _) as e -> e
    | Register r -> Register (renamed r)
    | Add (e1, e2) -> Add (helper e1, helper e2)
    | Sub (e1, e2) -> Sub (helper e1, helper e2)
    | Mult (e1, e2) -> Mult (helper e1, helper e2)
    | Div (e1, e2) -> Div (helper e1, helper e2)
    | And (e1, e2) -> And (helper e1, helper e2)
    | Or (e1, e2) -> Or (helper e1, helper e2)
    | Mod (e1, e2) -> Mod (helper e1, helper e2)
    | Compare (op, e1, e2) -> Compare (op, helper e1, helper e2)
    | Not e -> Not (helper e)
    | If_expr { conds; default } ->
        If_expr
          {
            conds = List.map conds ~f:(fun (c, e) -> (helper c, helper e));
            default = helper default;
          }
  in
  helper e

let merge_override =
  Map.merge ~f:(fun ~key:_ -> function
    | `Left v
    | `Right v
    (* always take the right value *)
    | `Both (_, v) ->
        Some v)

let compile
    ({ functions; main; desmos_vars; desmos_plots; status = `Valid } :
      [ `Valid ] C_style_separated_functions.t) : C_style_registers.t =
  let register_gen = Register_generator.create "local" in
  Register_generator.reset register_gen;
  let global_registers = Register.Hash_set.create () in
  let create_register ~local_set ~old_reg () =
    let old_name = Register.to_string old_reg in
    let reg = Register_generator.generate ~desc:old_name register_gen in
    Hash_set.add local_set reg;
    reg
  in
  let register_global reg =
    Hash_set.add global_registers reg;
    reg
  in
  let rec rename_in_stmts ~parent_names ~is_global ~local_set stmts =
    let helper ~cur_names =
      let combined_names = merge_override parent_names cur_names in
      let renamed = Map.find_exn combined_names in
      let child_scope_parent_names = merge_override parent_names cur_names in
      function
      | C_style_separated_functions.Decl reg ->
          let new_reg =
            if is_global then register_global reg
            else create_register ~local_set ~old_reg:reg ()
          in
          let cur_names = Map.add_exn cur_names ~key:reg ~data:new_reg in
          ([], cur_names)
      | If { branches; else_ } ->
          ( [
              C_style_registers.If
                {
                  branches =
                    List.map branches ~f:(fun (cond, body) ->
                        ( rename_in_expr ~renamed cond,
                          rename_in_stmts ~parent_names:child_scope_parent_names
                            ~is_global:false ~local_set body
                          |> fst ));
                  else_ =
                    rename_in_stmts ~parent_names:child_scope_parent_names
                      ~is_global:false ~local_set else_
                    |> fst;
                };
            ],
            cur_names )
      | While { cond; body } ->
          ( [
              While
                {
                  cond = rename_in_expr ~renamed cond;
                  body =
                    rename_in_stmts ~parent_names:child_scope_parent_names
                      ~is_global:false ~local_set body
                    |> fst;
                };
            ],
            cur_names )
      | Call { func_name; args; ret } ->
          ( [
              Call
                {
                  func_name;
                  args = List.map args ~f:(rename_in_expr ~renamed);
                  ret = Option.map ret ~f:renamed;
                };
            ],
            cur_names )
      | Return e -> ([ Return (rename_in_expr ~renamed e) ], cur_names)
      | Set (r, e) -> ([ Set (renamed r, rename_in_expr ~renamed e) ], cur_names)
    in
    List.fold_left stmts
      ~f:(fun (stmts, cur_names) stmt ->
        let new_stmts, cur_names = helper ~cur_names stmt in
        (stmts @ new_stmts, cur_names))
      ~init:([], Register.Map.empty)
  in
  let desmos_decl_names =
    List.fold desmos_vars ~init:Register.Map.empty
      ~f:(fun map (d : C_style_separated_functions.desmos_decl) ->
        Map.add_exn map ~key:d.reg ~data:(register_global d.reg))
  in
  let main, global_names =
    rename_in_stmts ~parent_names:desmos_decl_names ~is_global:true
      ~local_set:global_registers main
  in
  let global_names = merge_override desmos_decl_names global_names in
  {
    main;
    desmos_vars;
    desmos_plots;
    functions =
      Map.map functions ~f:(fun { params; body } ->
          let func_registers = Register.Hash_set.create () in
          let param_names =
            List.fold params ~init:Register.Map.empty ~f:(fun map reg ->
                Map.add_exn map ~key:reg
                  ~data:
                    (create_register ~local_set:func_registers ~old_reg:reg ()))
          in
          let parent_names = merge_override global_names param_names in
          let params_renamed = List.map params ~f:(Map.find_exn param_names) in
          let body_compiled =
            rename_in_stmts ~parent_names ~is_global:false
              ~local_set:func_registers body
            |> fst
          in
          let local_registers = Register.Set.of_hash_set func_registers in
          {
            C_style_registers.params = params_renamed;
            body = body_compiled;
            local_registers;
          });
    global_registers = Register.Set.of_hash_set global_registers;
  }
