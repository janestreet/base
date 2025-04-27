open! Import
include Source_code_position0

include%template
  Comparable.Make_using_comparator [@modality portable] (Source_code_position0)
