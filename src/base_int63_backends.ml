open! Import

let raise_s = Error.raise_s

module type Int_or_more = sig
  type t [@@deriving hash]
  include Int_intf.S with type t := t
  val of_int : int -> t
  val to_int : t -> int option
  val of_float_unchecked : float -> t
end

module Native : Int_or_more with type t = private int = struct
  include Base_int
  let to_int x = Some x
end

module Emulated : Int_or_more with type t = Base_int63_emul.t = Base_int63_emul
