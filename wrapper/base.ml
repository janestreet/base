include (Shadow_stdlib
         : module type of struct include Shadow_stdlib end
         (* Modules defined in Base *)
         with module Array     := Array
         with module Buffer    := Buffer
         with module Char      := Char
         with module Hashtbl   := Hashtbl
         with module Int32     := Int32
         with module Int64     := Int64
         with module Lazy      := Lazy
         with module List      := List
         with module Map       := Map
         with module Nativeint := Nativeint
         with module Printf    := Printf
         with module Random    := Random
         with module Set       := Set
         with module String    := String

         (* Support for generated lexers *)
         with module Lexing    := Lexing

         with type ('a, 'b, 'c) format              := ('a, 'b, 'c) format
         with type ('a, 'b, 'c, 'd) format4         := ('a, 'b, 'c, 'd) format4
         with type ('a, 'b, 'c, 'd, 'e, 'f) format6 := ('a, 'b, 'c, 'd, 'e, 'f) format6
        )
include Base0.Std
