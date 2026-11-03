open! Import0
include Source_code_position0

include%template
  Comparable.Make_using_comparator [@mode local] [@modality portable]
    (Source_code_position0)
