#include <assert.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <string.h>

CAMLprim value base_array_unsafe_float_blit(value src, value src_pos, value dst,
                                            value dst_pos, value len) {
  /* On both 32bit and 64bit boxes, floats are 64bits long and type
     casting the pointer to double achieves this.
  */
  memmove((double *)dst + Long_val(dst_pos), (double *)src + Long_val(src_pos),
          Long_val(len) * sizeof(double));

  return Val_unit;
}
