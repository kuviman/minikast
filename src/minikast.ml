module All_in_one = struct
  type value =
    | Int of int
    | String of string
    | Error

  type expr =
    | Const of value
    | Add of expr * expr

  let rec eval : expr -> value = function
    | Const value -> value
    | Add (a, b) -> (
        match (eval a, eval b) with
        | Int a, Int b -> Int (a + b)
        | String a, String b -> String (a ^ b)
        | _ -> Error)
end

type formatter = Format.formatter

let fprintf = Format.fprintf

module Abstract = struct
  module type Interpreter = sig
    type value
    type expr

    val eval : expr -> value
  end
end

module Error = struct
  module Value = struct
    type t = unit

    let make () : t = ()
    let print (fmt : formatter) (() : t) : unit = fprintf fmt "<error>"
  end
end

module Parts = struct
  module Const = struct
    module Expr = struct
      type 'v t = { value : 'v }

      let eval : 'v t -> 'v = fun { value } -> value
    end
  end

  module Int = struct
    module Value = struct
      type t = int

      let add : t -> t -> t = ( + )
      let print (fmt : formatter) (value : t) : unit = fprintf fmt "%d" value
    end
  end

  module Add = struct
    type addable =
      | Error of Error.Value.t
      | Int of Int.Value.t

    let add : addable -> addable -> addable =
     fun a b ->
      match (a, b) with
      | Error _, _ | _, Error _ -> Error (Error.Value.make ())
      | Int a, Int b -> Int (a + b)

    module type ValueS = sig
      type t

      val into_addable : t -> addable
      val from_addable : addable -> t
    end

    module type S = sig
      module I : Abstract.Interpreter

      module Expr : sig
        type t = {
          lhs : I.expr;
          rhs : I.expr;
        }
      end

      val eval : Expr.t -> I.value
    end

    module Make (I : Abstract.Interpreter) (V : ValueS with type t = I.value) :
      S with module I = I = struct
      module I = I

      module Expr = struct
        type t = {
          lhs : I.expr;
          rhs : I.expr;
        }
      end

      let eval : Expr.t -> I.value =
       fun { lhs; rhs } ->
        add (I.eval lhs |> V.into_addable) (I.eval rhs |> V.into_addable)
        |> V.from_addable
    end
  end

  module String = struct
    module Value = struct
      type t = string

      let add : t -> t -> t = ( ^ )
      let print (fmt : formatter) (value : t) : unit = fprintf fmt "%S" value
    end
  end
end

module type Printable = sig
  type t

  val print : formatter -> t -> unit
end

module Combined = struct
  module rec Unused : sig end = struct end

  and Value : sig
    type t =
      | Int of Parts.Int.Value.t
      | String of Parts.String.Value.t
      | Error of Error.Value.t

    include Parts.Add.ValueS with type t := t
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

    let print (fmt : formatter) (value : t) : unit =
      match value with
      | Int value -> Parts.Int.Value.print fmt value
      | String value -> Parts.String.Value.print fmt value
      | Error value -> Error.Value.print fmt value
  end

  and Expr : sig
    type t =
      | Const of Value.t Parts.Const.Expr.t
      | Add of Add.Expr.t
  end = struct
    type t =
      | Const of Value.t Parts.Const.Expr.t
      | Add of Add.Expr.t
  end

  and Interpreter :
    (Abstract.Interpreter with type value = Value.t and type expr = Expr.t) =
  struct
    type _unused = unit
    and expr = Expr.t
    and value = Value.t

    let rec _unused = ()

    and eval : expr -> value = function
      | Expr.Const expr -> Parts.Const.Expr.eval expr
      | Expr.Add expr -> Add.eval expr
  end

  and Add : (Parts.Add.S with type I.value = Value.t and type I.expr = Expr.t) =
    Parts.Add.Make (Interpreter) (Value)
end

open Combined

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
