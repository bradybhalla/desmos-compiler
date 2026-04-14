open! Core

module Register =
  String_id.Make
    (struct
      let module_name = "Register"
    end)
    ()

module Label =
  String_id.Make
    (struct
      let module_name = "Label"
    end)
    ()

module Function_name =
  String_id.Make
    (struct
      let module_name = "Function_name"
    end)
    ()

module Compare_op = struct
  type t = Eq | Lt | Gt | Ge | Le | Ne [@@deriving sexp]
end
