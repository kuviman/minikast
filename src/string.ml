open Prelude

module Value = struct
  type t = string

  let add : t -> t -> t = ( ^ )
  let print (fmt : formatter) (value : t) : unit = fprintf fmt "%S" value
end
