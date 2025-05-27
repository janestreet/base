#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

// only used for public release;
// internally this is implemented by caml_obj_is_stack in compiler runtime
CAMLprim value caml_dummy_obj_is_stack(__attribute__((unused)) value blk) {
  // Public compilers don't support stack allocation, so we always return 0
  return Val_int(0);
}

// only used for public release;
// internally this is implemented by caml_succ_scannable_prefix_len in compiler runtime
CAMLprim value caml_dummy_succ_scannable_prefix_len(__attribute__((unused)) value blk) {
  // Public compilers don't support mixed blocks, so we always return 0
  return Val_int(0);
}
