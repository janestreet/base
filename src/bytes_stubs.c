#include <caml/alloc.h>

/* This is the same as caml_create_local_bytes, except that we skip the
   bounds-check and instead do it on the ocaml side, so that we can mark the C
   call noalloc. */
CAMLprim value Base_unsafe_create_local_bytes(value len) {
  mlsize_t size = Long_val(len);
  return caml_alloc_string(size);
}
