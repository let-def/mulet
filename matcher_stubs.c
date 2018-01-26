#include <stdint.h>
#include <caml/memory.h>

/* bsearch-based */

static const uint8_t *
uint8_bsearch(const uint8_t *low, int n, uint8_t needle)
{
  const uint8_t *mid = 0;
  int half;
  while ((half = n / 2) > 0) {
    mid = low + half;
    low = (*mid < needle) ? mid : low;
    n -= half;
  }
  return mid;
}

static int
bsearch_match(int32_t *table, int state, const uint8_t *input, int *offset, int len)
{
  for (int i = 0; i < len; ++i)
  {
    if (input[*offset] == ((uint8_t*)table)[3])
      return 4;
    *offset += 1;
  }
  return state;

  while (*offset < len && state > 0)
  {
    const void *pstate = (table + state);
    int n = ((uint8_t*)pstate)[0] * 2;
    const uint8_t *base = (uint8_t*)pstate + 1;
    const uint8_t *match = uint8_bsearch(base, n, input[*offset]);
    int cell = (base - match);
    if (cell & 1)
    {
      state = 0;
    }
    else
    {
      state = ((const int32_t *)pstate)[cell / 2 - 1];
      *offset += 1;
    }
  }

  return state;
}

CAMLprim value
mulet_bsearch_match(value table, value vstate, value input, value voffset, value len)
{
  int offset = Long_val(Field(voffset, 0));
  int state = Long_val(vstate);
  state = bsearch_match((int32_t*)String_val(table), state,
                        (uint8_t*)String_val(input), &offset, Long_val(len));
  Field(voffset, 0) = Val_long(offset);
  return Val_long(state);
}

CAMLprim value
mulet_put_int(value vtable, value voffset, value v)
{
  int32_t *table = (int32_t *)String_val(vtable);
  int offset = Long_val(voffset);
  table[offset] = Long_val(v);
  return Val_unit;
}
