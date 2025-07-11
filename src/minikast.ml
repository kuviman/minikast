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

    module Dep = struct
      module type Value = sig
        type t

        val into_addable : t -> addable
        val from_addable : addable -> t
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
          (K.Interpreter.eval lhs |> K.Value.into_addable)
          (K.Interpreter.eval rhs |> K.Value.into_addable)
        |> K.Value.from_addable
    end
  end

  module Mul = struct
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

    include Parts.Add.Dep.Value with type t := t
    include Parts.Mul.Dep.Value with type t := t
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

      include Parts.Add.Dep.Value with type t := t
      include Parts.Mul.Dep.Value with type t := t
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
