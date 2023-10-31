#include <caml/alloc.h>
#include <caml/mlvalues.h>
// only used for public release;
// internally this is implemented as a compiler primitive
CAMLprim value caml_get_header0(value blk) {
  // undefined behaviour if blk is not a block
  intnat r = Hd_val(blk);
  return caml_copy_nativeint(r);
}
