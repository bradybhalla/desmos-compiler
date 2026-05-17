open! Core
open! Languages
open! Types
open Desmos_virtual_machine
module RegHashtbl = Hashtbl.Make (Register)

(* TODO: make this type more sane maybe with more types defined inside of it.
   If we get rid of Exit in Desmos_virtual_machine then we won't need to have
   the option there any more and it will be nicer *)
type instr = (Register.t * generalized_set_action) list option

type t = {
  instrs : instr array;
  label_line_lookup : int Label.Map.t;
  registers : (Register.t, float) Hashtbl.t;
  register_stacks : (Register.t, float list) Hashtbl.t;
  mutable program_counter : int;
}

(* TODO brady: support datatypes explicitly instead of converting to num *)
let evaluate_initial_register_value = function
  | Num n -> n
  | Bool b -> if b then 1. else 0.
  | Add (_, _)
  | Sub (_, _)
  | Mult (_, _)
  | Div (_, _)
  | Mod (_, _)
  | And (_, _)
  | Or (_, _)
  | Not _ | Register _ | LabelLineNumber _ | If_expr _
  | Compare (_, _, _) ->
      failwith "initial expression should be a constant"

let create program =
  (* initialize registers *)
  let registers = RegHashtbl.create () in
  let register_stacks = RegHashtbl.create () in
  Map.iteri program.info ~f:(fun ~key:r ~data:init_expr ->
      Hashtbl.add_exn registers ~key:r
        ~data:(evaluate_initial_register_value init_expr);
      Hashtbl.add_exn register_stacks ~key:r ~data:[]);
  (* build program representation *)
  let _, label_line_lookup, rev_instrs =
    List.fold_left program.main ~init:(0, Label.Map.empty, [])
      ~f:(fun (index, label_lookup, rev_instrs) block ->
        let label_lookup =
          Map.add_exn label_lookup ~key:block.label ~data:index
        in
        List.fold_left block.body ~init:(index, label_lookup, rev_instrs)
          ~f:(fun (index, label_lookup, rev_instrs) -> function
          | Instruction i -> (index + 1, label_lookup, Some i :: rev_instrs)
          | Exit -> (index + 1, label_lookup, None :: rev_instrs)))
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
  | Register r -> Hashtbl.find_exn t.registers r
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
  | If_expr { conds; default } ->
      let rec eval_conds = function
        | [] -> eval_expr ~t default
        | (cond, expr) :: rest ->
            if is_true (eval_expr ~t cond) then eval_expr ~t expr
            else eval_conds rest
      in
      eval_conds conds

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
      (* read next value of program counter from its register *)
      t.program_counter <-
        Hashtbl.find_exn t.registers program_counter_reg
        |> Float.round_nearest |> Float.to_int;
      `Not_done

(* Make this language type more sane *)
let run_until_done vm_prog =
  let t = create vm_prog in
  let rec loop () = match step t with `Done -> t | `Not_done -> loop () in
  loop ()

let inspect_register t reg = Hashtbl.find_exn t.registers reg
