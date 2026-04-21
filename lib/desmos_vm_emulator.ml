open! Core
open! Languages
open! Types
module RegHashtbl = Hashtbl.Make (Register)

(* TODO: make this type more sane *)
type instr = (Register.t * Desmos_virtual_machine.generalized_set) list option

type t = {
  instrs : instr array;
  label_line_lookup : int Label.Map.t;
  registers : (Register.t, float) Hashtbl.t;
  register_stacks : (Register.t, float list) Hashtbl.t;
  mutable program_counter : int;
}

let create (program : Register.Set.t Languages.Desmos_virtual_machine.t) =
  (* initialize registers *)
  let registers = RegHashtbl.create () in
  let register_stacks = RegHashtbl.create () in
  Set.iter program.info ~f:(fun r ->
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
      | Instruction i -> (index + 1, label_lookup, Some i :: rev_instrs)
      | Exit -> (index + 1, label_lookup, None :: rev_instrs))
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
  | LabelLineNumber lbl -> Float.of_int (Map.find_exn t.label_line_lookup lbl)
  | If_expr { conds; default } ->
      let rec eval_conds = function
        | [] -> eval_expr ~t default
        | (cond, expr) :: rest ->
            if is_true (eval_cond cond ~t) then eval_expr ~t expr
            else eval_conds rest
      in
      eval_conds conds

and eval_cond cond ~t =
  match cond with
  | Desmos_virtual_machine.BoolVal e -> eval_expr ~t e
  | Compare (op, a, b) ->
      let a = eval_expr ~t a in
      let b = eval_expr ~t b in
      let result =
        match op with
        | Compare_op.Lt -> Float.(a < b)
        | Gt -> Float.(a > b)
        | Le -> Float.(a <= b)
        | Ge -> Float.(a >= b)
        | Eq -> a ==. b
        | Ne -> not (a ==. b)
      in
      if result then 1. else 0.

let step t =
  match Array.get t.instrs t.program_counter with
  | None -> `Done
  | Some sets ->
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
              Hashtbl.set t.register_stacks ~key:reg
                ~data:(List.tl_exn cur_stack));
      (* set new values *)
      List.iter values_to_set ~f:(fun (reg, value) ->
          Hashtbl.set t.registers ~key:reg ~data:value);
      (* update pc *)
      t.program_counter <-
        Hashtbl.find_exn t.registers program_counter_reg
        |> Float.round_nearest |> Float.to_int;
      `Not_done

(* Make this language type more sane *)
let run_until_done (vm_prog : Register.Set.t Desmos_virtual_machine.t) =
  let t = create vm_prog in
  let rec loop () = match step t with `Done -> t | `Not_done -> loop () in
  loop ()
