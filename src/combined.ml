open Prelude

module rec Unused : sig end = struct end

and Value : sig
  type t =
    | Int of Parts.Int.Value.t
    | String of Parts.String.Value.t
    | Error of Error.Value.t

  val into_addable : t -> Parts.Add.addable
  val from_addable : Parts.Add.addable -> t
  val into_mulable : t -> Parts.Mul.mulable
  val from_mulable : Parts.Mul.mulable -> t

  include Printable with type t := t
end = struct
  type t =
    | Int of Parts.Int.Value.t
    | String of Parts.String.Value.t
    | Error of Error.Value.t

  let into_addable (value : t) : Parts.Add.addable =
    let fail () : Parts.Add.addable = Error (Error.Value.make ()) in
    match value with
    | Int x -> Int x
    | Error x -> Error x
    | String _ -> fail ()

  let from_addable (value : Parts.Add.addable) : t =
    match value with
    | Int x -> Int x
    | Error x -> Error x

  let into_mulable (value : t) : Parts.Mul.mulable =
    let fail () : Parts.Mul.mulable = Error (Error.Value.make ()) in
    match value with
    | Int x -> Int x
    | Error x -> Error x
    | String _ -> fail ()

  let from_mulable (value : Parts.Mul.mulable) : t =
    match value with
    | Int x -> Int x
    | Error x -> Error x

  let print (fmt : formatter) (value : t) : unit =
    match value with
    | Int value -> Parts.Int.Value.print fmt value
    | String value -> Parts.String.Value.print fmt value
    | Error value -> Error.Value.print fmt value
end

and Expr : sig
  type t =
    | Const of Const.Expr.t
    | Add of Add.Expr.t
    | Mul of Mul.Expr.t
end = struct
  type t =
    | Const of Const.Expr.t
    | Add of Add.Expr.t
    | Mul of Mul.Expr.t
end

and Interpreter : sig
  val eval : Expr.t -> Value.t
end = struct
  let eval : Expr.t -> Value.t = function
    | Expr.Const expr -> Const.eval expr
    | Expr.Add expr -> Add.eval expr
    | Expr.Mul expr -> Mul.eval expr
end

and Add :
  (Parts.Add.S with type K.Value.t = Value.t and type K.Expr.t = Expr.t) =
  Parts.Add.Make (Kast)

and Mul :
  (Parts.Mul.S with type K.Value.t = Value.t and type K.Expr.t = Expr.t) =
  Parts.Mul.Make (Kast)

and Const : (Parts.Const.S with type K.Value.t = Value.t) =
  Parts.Const.Make (Kast)

and Kast : sig
  module Value : sig
    type t

    val into_addable : t -> Parts.Add.addable
    val from_addable : Parts.Add.addable -> t
    val into_mulable : t -> Parts.Mul.mulable
    val from_mulable : Parts.Mul.mulable -> t
  end

  module Expr : sig
    type t
  end

  module Interpreter : sig
    val eval : Expr.t -> Value.t
  end
end = struct
  module Value = Value
  module Expr = Expr
  module Interpreter = Interpreter
end
