include Sexplib
module Lazy_group_id = Sexplib0.Lazy_group_id

module Option = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
end
