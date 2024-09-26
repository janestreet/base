#include <string.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static inline mlsize_t string_concat_from_array(value v_string_array, mlsize_t i,
                                                char *restrict result_contents) {
  value string = Field(v_string_array, i);
  mlsize_t len = caml_string_length(string);
  memcpy(result_contents, String_val(string), len);
  return len;
}

CAMLprim value Base_string_concat_array(value v_string_array, value v_sep) {
  CAMLparam2(v_string_array, v_sep);
  CAMLlocal1(result);
  const mlsize_t array_len = Wosize_val(v_string_array);
  if (array_len == 0) {
    result = caml_alloc_string(0);
    CAMLreturn(result);
  }
  const mlsize_t sep_len = caml_string_length(v_sep);
  mlsize_t string_len = (array_len - 1) * sep_len;
  for (mlsize_t i = 0; i < array_len; i++) {
    string_len += caml_string_length(Field(v_string_array, i));
  }
  result = caml_alloc_string(string_len);
  // This is freshly allocated and therefore safe to mark as [restrict]
  char *restrict result_contents = (char *)(String_val(result));
  if (sep_len == 0) {
    for (mlsize_t i = 0; i < array_len; i++) {
      result_contents += string_concat_from_array(v_string_array, i, result_contents);
    }
  } else {
    const char *sep = String_val(v_sep);
    result_contents += string_concat_from_array(v_string_array, 0, result_contents);
    for (mlsize_t i = 1; i < array_len; i++) {
      memcpy(result_contents, sep, sep_len);
      result_contents += sep_len;
      result_contents += string_concat_from_array(v_string_array, i, result_contents);
    }
  }
  CAMLreturn(result);
}
