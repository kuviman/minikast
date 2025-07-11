open Prelude

type mulable =
  | Error of Error.Value.t
  | Int of Int.Value.t

let add : mulable -> mulable -> mulable =
 fun a b ->
  match (a, b) with
  | Error _, _ | _, Error _ -> Error (Error.Value.make ())
  | Int a, Int b -> Int (a + b)

module Dep = struct
  module type Value = sig
    type t

    val into_mulable : t -> mulable
    val from_mulable : mulable -> t
  end

  module type Kast = sig
    module Value : Value
    include Abstract.Kast with module Value := Value
  end
end

module type S = sig
  module K : Dep.Kast

  module Expr : sig
    type t = {
      lhs : K.Expr.t;
      rhs : K.Expr.t;
    }
  end

  val eval : Expr.t -> K.Value.t
end

module Make (K : Dep.Kast) : S with module K = K = struct
  module K = K

  module Expr = struct
    type t = {
      lhs : K.Expr.t;
      rhs : K.Expr.t;
    }
  end

  let eval : Expr.t -> K.Value.t =
   fun { lhs; rhs } ->
    add
      (K.Interpreter.eval lhs |> K.Value.into_mulable)
      (K.Interpreter.eval rhs |> K.Value.into_mulable)
    |> K.Value.from_mulable
end
