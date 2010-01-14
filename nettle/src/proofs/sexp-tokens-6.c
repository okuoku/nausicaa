/* sexp-tokens-6.c --

   Raw tokenisation for a sexp with 2 number atoms as subsexps.
*/

#include "sexp-tokens.h"

static void
print_number (struct sexp_iterator * I)
{
  const char *	error_message;
  uint32_t	n;
  assert(SEXP_ATOM == I->type);
  E(sexp_iterator_get_uint32(I, &n), "parsing number");
  printf("number atom: %d\n", n);fflush(stdout);
  return;
 error:
  fprintf(stderr, "parse error: %s\n", error_message);
  exit(EXIT_FAILURE);
}

int
main (void)
{
  const char *		error_message;
  struct sexp_iterator	I[1];
#define SEXLEN		9
  static const char	sex[1+SEXLEN] = "(" "1:\x00" "2:\x01\x02" ")";

  E(sexp_iterator_first(I, SEXLEN, (const uint8_t *)sex),
    "initialising the iterator");
  E(sexp_iterator_enter_list(I), "entering list");
  {
    print_number(I);
    print_number(I);
    assert(SEXP_END == I->type);
  }
  E(sexp_iterator_exit_list(I), "exiting list");
  E(sexp_iterator_next(I), "advance to end");
  assert(SEXP_END == I->type);

  exit(EXIT_SUCCESS);
 error:
  fprintf(stderr, "parse error: %s\n", error_message);
  exit(EXIT_FAILURE);
}

/* end of file */
