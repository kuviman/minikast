open Prelude

module type S = sig
  module K : Abstract.Kast

  module Expr : sig
    type t = { value : K.Value.t }
  end

  val eval : Expr.t -> K.Value.t
end

module Make (K : Abstract.Kast) : S with module K = K = struct
  module K = K

  module Expr = struct
    type t = { value : K.Value.t }
  end

  let eval : Expr.t -> K.Value.t = fun { value } -> value
end
