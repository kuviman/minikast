open Prelude
open Combined
module Play = Play

let () =
  let e : Expr.t =
    Add
      {
        lhs = Const { value = Int 2 };
        rhs =
          Add { lhs = Const { value = Int 3 }; rhs = Const { value = Int 4 } };
      }
  in
  let result = Interpreter.eval e in
  Format.printf "%a\n" Value.print result
