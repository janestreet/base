#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>

#ifdef Reserved_mixed_block_scannable_wosize_native

CAMLprim value Base_obj_new_mixed_block(value v_tag, value v_wosize, value v_scannable) {
  CAMLparam3(v_tag, v_wosize, v_scannable);
  mlsize_t wosize = Long_val(v_wosize);
  tag_t tag = Long_val(v_tag);
  mlsize_t scannable = Long_val(v_scannable);
  reserved_t reserved = Reserved_mixed_block_scannable_wosize_native(scannable);
  CAMLreturn(caml_alloc_with_reserved(wosize, tag, reserved));
}

#else

CAMLprim value Base_obj_new_mixed_block(value v_tag, value v_wosize, value v_scannable) {
  CAMLparam3(v_tag, v_wosize, v_scannable);
  mlsize_t wosize = Long_val(v_wosize);
  tag_t tag = Long_val(v_tag);
  CAMLreturn(caml_alloc(wosize, tag));
}

#endif

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
