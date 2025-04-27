#include <caml/mlvalues.h>
#include <caml/fail.h>


static const char *parse_sign_and_base(const char *p,
                                       /*out*/ int *base,
                                       /*out*/ int *signedness,
                                       /*out*/ int *sign) {
  *sign = 1;
  if (*p == '-') {
    *sign = -1;
    p++;
  } else if (*p == '+')
    p++;
  *base = 10;
  *signedness = 1;
  if (*p == '0') {
    switch (p[1]) {
    case 'x':
    case 'X':
      *base = 16;
      *signedness = 0;
      p += 2;
      break;
    case 'o':
    case 'O':
      *base = 8;
      *signedness = 0;
      p += 2;
      break;
    case 'b':
    case 'B':
      *base = 2;
      *signedness = 0;
      p += 2;
      break;
    case 'u':
    case 'U':
      *signedness = 0;
      p += 2;
      break;
    }
  }
  return p;
}

static int parse_digit(char c) {
  if (c >= '0' && c <= '9')
    return c - '0';
  else if (c >= 'A' && c <= 'F')
    return c - 'A' + 10;
  else if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  else
    return -1;
}

#define INT32_ERRMSG "Int32.of_string"
#define INT64_ERRMSG "Int64.of_string"
#define INTNAT_ERRMSG "Nativeint.of_string"

static intnat parse_intnat(value s, unsigned int nbits, const char *errmsg) {
  const char *p;
  uintnat res, threshold;
  int sign, base, signedness, d;

  p = parse_sign_and_base(String_val(s), &base, &signedness, &sign);
  threshold = ((uintnat)-1) / base;
  d = parse_digit(*p);
  if (d < 0 || d >= base)
    caml_failwith(errmsg);
  for (p++, res = d; /*nothing*/; p++) {
    char c = *p;
    if (c == '_')
      continue;
    d = parse_digit(c);
    if (d < 0 || d >= base)
      break;
    /* Detect overflow in multiplication base * res */
    if (res > threshold)
      caml_failwith(errmsg);
    res = base * res + d;
    /* Detect overflow in addition (base * res) + d */
    if (res < (uintnat)d)
      caml_failwith(errmsg);
  }
  if (p != String_val(s) + caml_string_length(s)) {
    caml_failwith(errmsg);
  }
  if (signedness) {
    /* Signed representation expected, allow -2^(nbits-1) to 2^(nbits-1) - 1 */
    if (sign >= 0) {
      if (res >= (uintnat)1 << (nbits - 1))
        caml_failwith(errmsg);
    } else {
      if (res > (uintnat)1 << (nbits - 1))
        caml_failwith(errmsg);
    }
  } else {
    /* Unsigned representation expected, allow 0 to 2^nbits - 1
       and tolerate -(2^nbits - 1) to 0 */
    if (nbits < sizeof(uintnat) * 8 && res >= (uintnat)1 << nbits)
      caml_failwith(errmsg);
  }
  return sign < 0 ? -((intnat)res) : (intnat)res;
}

// only used for public release;
// internally this is implemented by caml_int32_of_string_unboxed
CAMLprim int32_t caml_dummy_int32_of_string_unboxed(value s) {
  return (int32_t)parse_intnat(s, 32, INT32_ERRMSG);
}

// only used for public release;
// internally this is implemented by caml_int64_of_string_unboxed
CAMLprim int64_t caml_dummy_int64_of_string_unboxed(value s) {
  const char *p;
  uint64_t res, threshold;
  int sign, base, signedness, d;

  p = parse_sign_and_base(String_val(s), &base, &signedness, &sign);
  threshold = ((uint64_t)-1) / base;
  d = parse_digit(*p);
  if (d < 0 || d >= base)
    caml_failwith(INT64_ERRMSG);
  res = d;
  for (p++; /*nothing*/; p++) {
    char c = *p;
    if (c == '_')
      continue;
    d = parse_digit(c);
    if (d < 0 || d >= base)
      break;
    /* Detect overflow in multiplication base * res */
    if (res > threshold)
      caml_failwith(INT64_ERRMSG);
    res = base * res + d;
    /* Detect overflow in addition (base * res) + d */
    if (res < (uint64_t)d)
      caml_failwith(INT64_ERRMSG);
  }
  if (p != String_val(s) + caml_string_length(s)) {
    caml_failwith(INT64_ERRMSG);
  }
  if (signedness) {
    /* Signed representation expected, allow -2^63 to 2^63 - 1 only */
    if (sign >= 0) {
      if (res >= (uint64_t)1 << 63)
        caml_failwith(INT64_ERRMSG);
    } else {
      if (res > (uint64_t)1 << 63)
        caml_failwith(INT64_ERRMSG);
    }
  }
  if (sign < 0)
    res = -res;
  return res;
}

// only used for public release;
// internally this is implemented by caml_nativeint_of_string_unboxed
CAMLprim intnat caml_dummy_nativeint_of_string_unboxed(value s) {
  return parse_intnat(s, 8 * sizeof(value), INTNAT_ERRMSG);
}
