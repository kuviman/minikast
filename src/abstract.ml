open Prelude

module type Kast = sig
  module Value : sig
    type t
  end

  module Expr : sig
    type t
  end

  module Interpreter : sig
    val eval : Expr.t -> Value.t
  end
end
