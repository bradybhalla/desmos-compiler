open! Core
open! Languages
open! Types

(* TODO: this pass got kind of complicated because I wanted to have the blocks work a certain way for future optimizations. hopefully it actually works otherwise I made it more annoying than I had to for nothing. *)

let label_gen = Label_generator.create "explicate_control"

(* TODO: figure out how to compile away units *)
let rec compile_expr = function
  | C_style_separated_functions.Unit -> failwith "TODO: unit"
  | Register reg -> Register_func_instrs.Register reg
  | Num n -> Num n
  | Bool b -> Bool b
  | Add (e1, e2) -> Add (compile_expr e1, compile_expr e2)
  | Sub (e1, e2) -> Sub (compile_expr e1, compile_expr e2)
  | Mult (e1, e2) -> Mult (compile_expr e1, compile_expr e2)
  | Div (e1, e2) -> Div (compile_expr e1, compile_expr e2)
  | And (e1, e2) -> And (compile_expr e1, compile_expr e2)
  | Or (e1, e2) -> Or (compile_expr e1, compile_expr e2)
  | Not e -> Not (compile_expr e)
  | Mod (e1, e2) -> Mod (compile_expr e1, compile_expr e2)
  | Compare (op, e1, e2) -> Compare (op, compile_expr e1, compile_expr e2)
  | If_expr { conds; default } ->
      If_expr
        {
          conds =
            List.map conds ~f:(fun (cond, result) ->
                (compile_expr cond, compile_expr result));
          default = compile_expr default;
        }

(* TODO: storing things as rev makes it slightly more efficent, but maybe I should just concat to keep it more readable. right now it is kind of hard to follow the logic.  *)
let rec compile_statements ~cur_label ~default_next ~stmts_rev ~blocks_rev =
  let open Register_func_instrs in
  function
  | [] -> (
      match default_next with
      | Some control_flow ->
          List.rev
            ({ label = cur_label; body = List.rev stmts_rev; control_flow }
            :: blocks_rev)
      | None ->
          (* we have an error checking pass that makes sure every function terminates. to sanity
             check ourselves we should only be able to get here if stmts_rev is empty *)
          if not (List.is_empty stmts_rev) then
            failwith
              "branch doesn't terminate (this should have been caught in the \
               error checking pass so there is a compiler bug somehwere)";

          (* we still need to add the block because the label might be referenced by the if statement exit. This block is unreachable. *)
          (* TODO: add a pass to detect unreachable blocks and remove them *)
          List.rev
            ({
               label = cur_label;
               body = List.rev stmts_rev;
               control_flow = Exit;
             }
            :: blocks_rev))
  | stmt :: rest -> (
      match stmt with
      | C_style_registers.Return expr ->
          List.rev
            ({
               label = cur_label;
               body = List.rev stmts_rev;
               control_flow = Return (compile_expr expr);
             }
            :: blocks_rev)
      | If { branches; else_ } ->
          let final_label =
            Label_generator.generate ~desc:"if_statement_exit" label_gen
          in
          let branch_conds, branch_blocks =
            List.map branches ~f:(fun (cond, stmts) ->
                let cur_label =
                  Label_generator.generate ~desc:"if_statement_branch" label_gen
                in
                let cond = compile_expr cond in
                let blocks =
                  compile_statements ~cur_label
                    ~default_next:
                      (Some (Jump { conds = []; default = final_label }))
                    ~stmts_rev:[] ~blocks_rev:[] stmts
                in
                ((cond, cur_label), blocks))
            |> List.unzip
          in
          let else_label, else_blocks =
            match else_ with
            | [] -> (final_label, [])
            | stmts ->
                let cur_label =
                  Label_generator.generate ~desc:"if_statement_else" label_gen
                in
                let blocks =
                  compile_statements ~cur_label
                    ~default_next:
                      (Some (Jump { conds = []; default = final_label }))
                    ~stmts_rev:[] ~blocks_rev:[] stmts
                in
                (cur_label, blocks)
          in
          let blocks_to_add =
            [
              {
                label = cur_label;
                body = List.rev stmts_rev;
                control_flow =
                  Jump { conds = branch_conds; default = else_label };
              };
            ]
            @ List.concat branch_blocks @ else_blocks
          in
          let blocks_rev = List.rev blocks_to_add @ blocks_rev in
          compile_statements ~cur_label:final_label ~default_next ~stmts_rev:[]
            ~blocks_rev rest
      | While { cond = cond_expr; body } ->
          let final_label =
            Label_generator.generate ~desc:"while_end" label_gen
          in
          let entry_label =
            Label_generator.generate ~desc:"while_entry" label_gen
          in
          let jump =
            Jump
              {
                conds = [ (compile_expr cond_expr, entry_label) ];
                default = final_label;
              }
          in
          let block_before_while =
            {
              label = cur_label;
              body = List.rev stmts_rev;
              control_flow = jump;
            }
          in
          let blocks_while_body =
            (* while body, prepare condition, and then jump *)
            compile_statements ~cur_label:entry_label ~default_next:(Some jump)
              ~stmts_rev:[] ~blocks_rev:[] body
          in
          let blocks_rev =
            List.rev (block_before_while :: blocks_while_body) @ blocks_rev
          in
          compile_statements ~cur_label:final_label ~default_next ~stmts_rev:[]
            ~blocks_rev rest
      | Set (reg, expr) ->
          let stmt = Set (reg, compile_expr expr) in
          compile_statements ~cur_label ~default_next
            ~stmts_rev:(stmt :: stmts_rev) ~blocks_rev rest
      | Call { func_name; args; ret } ->
          let stmt =
            Call { func_name; args = List.map ~f:compile_expr args; ret }
          in
          compile_statements ~cur_label ~default_next
            ~stmts_rev:(stmt :: stmts_rev) ~blocks_rev rest)

let compile C_style_registers.{ functions; main; registers } =
  Label_generator.reset label_gen;
  Register_func_instrs.
    {
      functions =
        Map.map functions ~f:(fun { params; body } ->
            let entry_label =
              Label_generator.generate ~desc:"function_entry" label_gen
            in
            let blocks =
              compile_statements ~cur_label:entry_label ~default_next:None
                ~stmts_rev:[] ~blocks_rev:[] body
            in
            { entry_label; params; blocks });
      main =
        (let entry_label = Label_generator.generate ~desc:"main" label_gen in
         compile_statements ~cur_label:entry_label ~default_next:(Some Exit)
           ~stmts_rev:[] ~blocks_rev:[] main);
      registers;
    }
