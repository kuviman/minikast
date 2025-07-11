open Prelude

module Value = struct
  type t = unit

  let make () : t = ()
  let print (fmt : formatter) (() : t) : unit = fprintf fmt "<error>"
end
