open Prelude

module Value = struct
  type t = int

  let add : t -> t -> t = ( + )
  let print (fmt : formatter) (value : t) : unit = fprintf fmt "%d" value
end
