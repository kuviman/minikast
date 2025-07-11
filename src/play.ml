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

module Expr = struct
  module Const = struct
    type 'v t = { value : 'v }

    let eval : 'v t -> 'v = fun { value } -> value
  end

  module Add = struct
    type 'e t = {
      lhs : 'e;
      rhs : 'e;
    }

    let eval : 'e 'v. eval:('e -> 'v) -> add:('v -> 'v -> 'v) -> 'e t -> 'v =
     fun ~eval ~add { lhs; rhs } -> add (eval lhs) (eval rhs)
  end
end

module Value = struct
  module type S = sig
    type t

    val add : t -> t -> t
  end

  module Int : S = struct
    type t = int

    let add : t -> t -> t = ( + )
  end

  module String : S = struct
    type t = string

    let add : t -> t -> t = ( ^ )
  end

  module type ErrorS = sig
    include S

    val make : unit -> t
  end

  module Error : ErrorS = struct
    type t = unit

    let make () : t = ()
    let add () () = ()
  end
end

module Combined = struct
  type _unused = unit

  and expr =
    | E_Const of value Expr.Const.t
    | E_Add of expr Expr.Add.t

  and value =
    | V_Int of Value.Int.t
    | V_String of Value.String.t
    | V_Error of Value.Error.t

  let rec _unused = ()

  and eval : expr -> value = function
    | E_Const expr -> Expr.Const.eval expr
    | E_Add expr -> Expr.Add.eval ~eval ~add expr

  and add : value -> value -> value =
   fun a b ->
    let fail () = V_Error (Value.Error.make ()) in
    match (a, b) with
    | V_Int a, V_Int b -> V_Int (Value.Int.add a b)
    | V_Int _, _ -> fail ()
    | V_String a, V_String b -> V_String (Value.String.add a b)
    | V_String _, _ -> fail ()
    | V_Error a, V_Error b -> V_Error (Value.Error.add a b)
    | V_Error _, _ -> fail ()
end
