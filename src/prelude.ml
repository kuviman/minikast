type formatter = Format.formatter

let fprintf = Format.fprintf

module type Printable = sig
  type t

  val print : formatter -> t -> unit
end
