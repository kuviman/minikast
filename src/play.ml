open Prelude

module Kast = struct
  module Value = struct
    type t = ..
    type print_fn = t -> (formatter -> unit) option

    let printers : print_fn list Atomic.t = Atomic.make []

    let register_print : print_fn -> unit =
     fun f -> Atomic.set printers (f :: Atomic.get printers)
  end

  module Expr = struct
    type t = ..
  end

  module Compiler = struct end

  module Interpreter = struct
    type t
    type eval_fn = Expr.t -> (t -> Value.t) option

    let eval_impls : eval_fn list ref = ref []

    let register_eval : eval_fn -> unit =
     fun f -> eval_impls := f :: !eval_impls

    let eval : t -> Expr.t -> Value.t =
     fun state expr ->
      (!eval_impls |> List.find_map (fun f -> f expr) |> Option.get) state
  end
end

module Plugins = struct
  module Int = struct
    type t = int
    type Kast.Value.t += Int of t

    let print : t -> formatter -> unit =
     fun value fmt -> Format.fprintf fmt "%d" value

    let () =
      Kast.Value.register_print (function
        | Int value -> Some (print value)
        | _ -> None)
  end

  module Add = struct
    type Kast.Value.t += Int = Int.Int

    type expr = {
      lhs : Kast.Expr.t;
      rhs : Kast.Expr.t;
    }

    type Kast.Expr.t += Add of expr

    let eval : expr -> Kast.Interpreter.t -> Kast.Value.t =
     fun { lhs; rhs } interpreter ->
      let lhs = Kast.Interpreter.eval interpreter lhs in
      let rhs = Kast.Interpreter.eval interpreter rhs in
      match (lhs, rhs) with
      | Int a, Int b -> Int (a + b)
      | _ -> failwith "can only add ints"

    let () =
      Kast.Interpreter.register_eval (function
        | Add expr -> Some (eval expr)
        | _ -> None)
  end
end
