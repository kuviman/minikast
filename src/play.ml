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

module Error = struct
  module Value = struct
    type t = unit

    let make () : t = ()
  end
end

module Const = struct
  module Expr = struct
    type 'v t = { value : 'v }

    let eval : 'v t -> 'v = fun { value } -> value
  end
end

module Add = struct
  module type ValueS = sig
    type t

    val add : t -> t -> t
  end

  module Expr = struct
    type 'e t = {
      lhs : 'e;
      rhs : 'e;
    }

    let eval : 'e 'v. eval:('e -> 'v) -> add:('v -> 'v -> 'v) -> 'e t -> 'v =
     fun ~eval ~add { lhs; rhs } -> add (eval lhs) (eval rhs)
  end
end

module Int = struct
  module Value = struct
    type t = int

    let add : t -> t -> t = ( + )
  end
end

module String = struct
  module Value = struct
    type t = string

    let add : t -> t -> t = ( ^ )
  end
end

module Combined = struct
  type _unused = unit

  and expr =
    | E_Const of value Const.Expr.t
    | E_Add of expr Add.Expr.t

  and value =
    | V_Int of Int.Value.t
    | V_String of String.Value.t
    | V_Error of Error.Value.t

  let rec _unused = ()

  and eval : expr -> value = function
    | E_Const expr -> Const.Expr.eval expr
    | E_Add expr -> Add.Expr.eval ~eval ~add expr

  and add : value -> value -> value =
   fun a b ->
    let fail () = V_Error (Error.Value.make ()) in
    match (a, b) with
    | V_Error _, _ | _, V_Error _ -> V_Error (Error.Value.make ())
    | V_Int a, V_Int b -> V_Int (Int.Value.add a b)
    | V_Int _, _ -> fail ()
    | V_String a, V_String b -> V_String (String.Value.add a b)
    | V_String _, _ -> fail ()
end
