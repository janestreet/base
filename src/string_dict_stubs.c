#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#define copy_block caml_copy_nativeint
#define Block_val(v) Nativeint_val(v)
typedef intnat block_t;

CAMLprim value Base_string_dict_blocks_of_string(value str)
{
  return str;
}

CAMLprim value Base_string_dict_num_blocks(value blocks)
{
  return Val_long(Wosize_val(blocks));
}

CAMLprim value Base_string_dict_get_block(value blocks, value vi)
{
  uintnat i = Unsigned_long_val(vi);
  if (i >= Wosize_val(blocks)) caml_invalid_argument("base_string_dict_get_block");
  return copy_block(((block_t*)blocks)[i]);
}

CAMLprim value Base_string_dict_make_blocks(value block_array)
{
  CAMLparam1(block_array);
  CAMLlocal1(result);
  mlsize_t i;
  mlsize_t size = Wosize_val(block_array);
  result = caml_alloc(size, Abstract_tag);
  for (i = 0; i < size; i++)
    ((block_t*)result)[i] = Block_val(Field(block_array, i));
  CAMLreturn(result);
}

#define num_children(t) Unsigned_long_val(Field(t, 0))
#define Keys(t)         ((block_t*)(Field(t, 1)))
#define Child(t, n)     Field(Field(t, 2), n)
#define Value(t)        Field(t, 3)

#if __GNUC__ >= 3
# define likely(x) __builtin_expect (!!(x), 1)
# define unlikely(x) __builtin_expect (!!(x), 0)
#else
# define likely(x) (x)
# define unlikely(x) (x)
#endif

/* If you touch this code, make sure to check the benchmarks in
   ../bench/bench_dict.ml */
CAMLprim value Base_string_dict_find(value t, value key)
{
  uintnat num_blocks = Wosize_val(key);
  block_t *input_blocks = (block_t*)key;
  for (; num_blocks; num_blocks--) {
    block_t input_block = *input_blocks++;
    block_t *keys = Keys(t);
    uintnat a = 0;
    uintnat b = num_children(t);
    /* It is likely that the first block is enough to distinguish all
       cases. So the remaining nodes will often form a chain. We
       optimize for this case. */
    if (b == 1) {
      if (likely(input_block == keys[0]))
        t = Child(t, 0);
      else
        return Val_int(0);
    } else {
      for (;;) {
        uintnat c;
        block_t block;
        if (unlikely(a >= b)) return Val_int(0);
        c = (a + b) >> 1;
        block = keys[c];
        if (input_block < block)
          b = c;
        else if (input_block > block)
          a = c + 1;
        else {
          t = Child(t, c);
          break;
        }
      }
    }
  }
  return Value(t);
}
