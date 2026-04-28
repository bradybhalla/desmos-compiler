open! Core
open! Languages
open! Types
open C_style_frontend

(* TODO: should probably encapsulate this is a module *)
let next_register_id = ref 0

let generate_register () =
  let res = [%string "0internal_%{!next_register_id#Int}"] in
  next_register_id := !next_register_id + 1;
  Register.of_string res

let reset_register_generator () = next_register_id := 0

let rec compile_expr = function
  | Unit -> ([], C_style_separated_functions.Unit)
  | Register reg -> ([], Register reg)
  | Num _ | Bool _
  | Add (_, _)
  | Sub (_, _)
  | Mult (_, _)
  | Div (_, _)
  | And (_, _)
  | Or (_, _)
  | Not _
  | Mod (_, _)
  | If_expr _
  | Call (_, _) ->
      failwith "TODO"

(* Compile normal statements, ignoring function definitions *)
let rec compile_stmt = function
  | Function_def _ -> []
  | Return expr ->
      let extracted_calls, expr = compile_expr expr in
      extracted_calls @ [ C_style_separated_functions.Return expr ]
  | If { branches = []; _ } -> failwith "empty if statement shouldn't exist"
  | If { branches = first_branch :: other_branches; else_ } ->
      (* TODO: once short-circuiting behavior has been fully decided this may need to be updated.
         Right now this assumes that simultaneous conditionals do short circuit like a normal
         language. *)
      let transform_branch ~allow_extracted_calls (cond, body) =
        let extracted_calls, cond = compile_expr cond in
        if not allow_extracted_calls then
          failwith
            "call expression found in non-first if statement conditional. this \
             should not be possible"
        else ();
        (extracted_calls, (cond, List.concat_map ~f:compile_stmt body))
      in
      (* It is possible there is a function call in the first conditional,
         but after the previous pass there should not be any function call
         in other conditionals. We check this here to catch any possible
         errors. *)
      let extracted_calls, first_branch =
        transform_branch ~allow_extracted_calls:true first_branch
      in
      let other_branches =
        other_branches
        |> List.map ~f:(transform_branch ~allow_extracted_calls:false)
        |> List.map ~f:snd
      in
      extracted_calls
      @ [
          C_style_separated_functions.If
            {
              branches = first_branch :: other_branches;
              else_ = List.concat_map ~f:compile_stmt else_;
            };
        ]
  | Set (reg, expr) ->
      let extracted_calls, expr = compile_expr expr in
      extracted_calls @ [ C_style_separated_functions.Set (reg, expr) ]
  | While (cond, stmts) ->
      [
        While
          {
            cond = compile_expr cond;
            body = List.concat_map ~f:compile_stmt stmts;
          };
      ]
  | Call (func_name, args) ->
      let extracted_calls, args =
        args |> List.map ~f:compile_expr |> List.unzip
      in
      let extracted_calls = List.concat extracted_calls in
      extracted_calls
      @ [ C_style_separated_functions.Call { func_name; args; ret = None } ]

let extract_and_compile_function_defs = function
  | Function_def (name, params, stmts) ->
      Some
        ( name,
          C_style_separated_functions.
            { params; body = List.concat_map ~f:compile_stmt stmts } )
  | Return _ | If _ | Set (_, _) | While (_, _) | Call (_, _) -> None

let compile program =
  reset_register_generator ();
  let functions =
    program
    |> List.filter_map ~f:extract_and_compile_function_defs
    |> Function_name.Map.of_alist_exn
  in
  {
    C_style_separated_functions.functions;
    main = List.concat_map ~f:compile_stmt program;
  }
