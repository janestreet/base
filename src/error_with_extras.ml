open! Import
include Error

module Utf8 = struct
  let to_string_hum (t : t) = Info.Utf8.to_string_hum (t :> Info.t)
  let to_string_mach (t : t) = Info.Utf8.to_string_mach (t :> Info.t)
end
