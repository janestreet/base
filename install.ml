#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"base"
  [ oasis_lib "base"
  ; oasis_lib "base0"
  ; oasis_lib "caml"
  ; oasis_lib "shadow_stdlib"
  ; file "META" ~section:"lib"
  ]
