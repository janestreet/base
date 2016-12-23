#include <stdint.h>
#include <caml/mlvalues.h>
CAMLexport uint32_t internalhash_fold_blob(uint32_t h, mlsize_t len, uint8_t *s);
