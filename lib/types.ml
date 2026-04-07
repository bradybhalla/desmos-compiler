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
