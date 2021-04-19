include Sexplib

module Option = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
end
