open! Core
open! Languages
open! Types
module RegHashtbl = Hashtbl.Make (Register)

type t = {
  instrs : Desmos_virtual_machine.instruction array;
  label_line_lookup : int Label.Map.t;
  registers : (Register.t, float) Hashtbl.t;
  register_stacks : (Register.t, float list) Hashtbl.t;
  mutable program_counter : int;
}

let create (program : Languages.Desmos_virtual_machine.t) =
  (* initialize registers *)
  let registers = RegHashtbl.create () in
  let register_stacks = RegHashtbl.create () in
  Set.iter program.registers ~f:(fun r ->
      Hashtbl.add_exn registers ~key:r ~data:0.;
      Hashtbl.add_exn register_stacks ~key:r ~data:[]);
  (* build program representation *)
  let _, label_line_lookup, rev_instrs =
    List.fold_left program.main ~init:(0, Label.Map.empty, [])
      ~f:(fun (index, label_lookup, rev_instrs) -> function
      (* add label at the current index *)
      | Label lbl ->
          (index, Map.add_exn label_lookup ~key:lbl ~data:index, rev_instrs)
      (* add instruction and increment index *)
      | Instruction i -> (index + 1, label_lookup, i :: rev_instrs))
  in
  {
    instrs = rev_instrs |> List.rev |> Array.of_list;
    label_line_lookup;
    registers;
    register_stacks;
    program_counter = 0;
  }

let ( ==. ) a b = Float.(abs (a -. b) < 0.001)
let is_true v = v ==. 1. (* bools represented as 0/1 *)

let rec eval_expr expr ~t =
  match expr with
  | Desmos_virtual_machine.Register r -> Hashtbl.find_exn t.registers r
  | ProgramCounter -> float_of_int t.program_counter
  | Num f -> f
  | Bool b -> if b then 1. else 0.
  | Add (a, b) -> eval_expr ~t a +. eval_expr ~t b
  | Sub (a, b) -> eval_expr ~t a -. eval_expr ~t b
  | Mult (a, b) -> eval_expr ~t a *. eval_expr ~t b
  | Div (a, b) -> eval_expr ~t a /. eval_expr ~t b
  | And (a, b) -> eval_expr ~t a *. eval_expr ~t b
  | Or (a, b) ->
      let a = eval_expr ~t a in
      let b = eval_expr ~t b in
      a +. b -. (a *. b)
  | Not a -> 1. -. eval_expr ~t a
  | Mod (a, b) -> Float.mod_float (eval_expr ~t a) (eval_expr ~t b)
  | Compare (op, a, b) ->
      let a = eval_expr ~t a in
      let b = eval_expr ~t b in
      let result =
        match op with
        | Compare_op.Eq -> Float.(a = b)
        | Lt -> Float.(a < b)
        | Gt -> Float.(a > b)
        | Ge -> Float.(a >= b)
        | Le -> Float.(a <= b)
        | Ne -> Float.(a <> b)
      in
      if result then 1. else 0.

let step t =
  let sets, pc_action = Array.get t.instrs t.program_counter in
  (* calculate next program counter *)
  let next_pc =
    match pc_action with
    | NextInstr -> t.program_counter + 1
    | Jump { conds; default } -> (
        let conditional_target =
          List.find_map conds ~f:(fun (cond_expr, target) ->
              if is_true (eval_expr ~t cond_expr) then Some target else None)
        in
        let target = Option.value conditional_target ~default in
        match target with
        | JumpToLabel lbl -> Map.find_exn t.label_line_lookup lbl
        | JumpToRegister reg ->
            (* TODO brady: should probably have type safe ints/floats in the future, this is more like how desmos does it but I want compiler guarantees I won't be jumping to line 13.5 *)
            Hashtbl.find_exn t.registers reg |> round |> int_of_float)
    | Exit -> -1
  in
  (* compute all expressions using the old values *)
  let values_to_set =
    List.filter_map sets ~f:(fun (reg, set) ->
        match set with
        | Set expr | PushAndSet expr -> Some (reg, eval_expr expr ~t)
        | Push | Pop -> None)
  in
  (* push/pop registers as needed *)
  List.iter sets ~f:(fun (reg, set) ->
      match set with
      | Set _ -> ()
      | PushAndSet _ | Push ->
          let cur_stack = Hashtbl.find_exn t.register_stacks reg in
          let cur_val = Hashtbl.find_exn t.registers reg in
          Hashtbl.set t.register_stacks ~key:reg ~data:(cur_val :: cur_stack)
      | Pop ->
          let cur_stack = Hashtbl.find_exn t.register_stacks reg in
          Hashtbl.set t.registers ~key:reg ~data:(List.hd_exn cur_stack);
          Hashtbl.set t.register_stacks ~key:reg ~data:(List.tl_exn cur_stack));
  (* set new values *)
  List.iter values_to_set ~f:(fun (reg, value) ->
      Hashtbl.set t.registers ~key:reg ~data:value);
  (* update pc *)
  t.program_counter <- next_pc;
  if t.program_counter < 0 then `Done else `Not_done

let run_until_done (vm_prog : Desmos_virtual_machine.t) =
  let t = create vm_prog in
  let rec loop () = match step t with `Done -> t | `Not_done -> loop () in
  loop ()
