open Prelude

module Dep = struct
  module type Value = sig
    type t
  end

  module type Kast = sig
    module Value : Value
  end
end

module type S = sig
  module K : Dep.Kast

  module Expr : sig
    type t = { value : K.Value.t }
  end

  val eval : Expr.t -> K.Value.t
end

module Make (K : Dep.Kast) : S with module K = K = struct
  module K = K

  module Expr = struct
    type t = { value : K.Value.t }
  end

  let eval : Expr.t -> K.Value.t = fun { value } -> value
end
