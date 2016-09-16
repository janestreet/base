/* This file is just preprocessed.  Lines of the form "OUT:XXX" are
   kept and replaced by XXX in the output to produce
   base_int63_backend.ml. */

#include <caml/mlvalues.h>

/* Defined in <caml/mlvalues.h> */
#if defined(ARCH_SIXTYFOUR)
"OUT:include Base_int63_backends.Native"
#else
"OUT:include Base_int63_backends.Emulated"
#endif
