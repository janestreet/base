#include <caml/alloc.h>
#include <caml/mlvalues.h>
// only used for public release;
// internally this is implemented by caml_dummy_obj_is_stack in compiler runtime
CAMLprim value caml_dummy_obj_is_stack(__attribute__((unused)) value blk) {
  // Public compilers don't support stack allocation, so we always return 0
  return Val_int(0);
}
