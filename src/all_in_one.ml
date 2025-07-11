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
