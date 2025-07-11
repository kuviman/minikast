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

    module type S = functor (I : Abstract.Interpreter) -> sig
      module Expr : sig
        type t
      end

      val eval : Expr.t -> I.value
    end

    module Make (I : Abstract.Interpreter) (V : ValueS with type t = I.value) =
    struct
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
    end
  end
end

module Combined = struct
  module rec Unused : sig end = struct end

  and Value : sig
    type t

    include Parts.Add.ValueS with type t := t
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
  end

  and Interpreter : (Abstract.Interpreter with type value = Value.t) = struct
    type _unused = unit

    and expr =
      | E_Const of value Parts.Const.Expr.t
      | E_Add of Add.Expr.t

    and value = Value.t

    let rec _unused = ()

    and eval : expr -> value = function
      | E_Const expr -> Parts.Const.Expr.eval expr
      | E_Add expr -> Add.eval expr
  end

  and Add : sig
    module Expr : sig
      type t
    end

    val eval : Expr.t -> Value.t
  end =
    Parts.Add.Make (Interpreter) (Value)
end
