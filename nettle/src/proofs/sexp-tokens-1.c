/* sexp-tokens-1.c --

   Raw tokenisation for a sexp with 3 atoms as subsexps.
*/

#include "sexp-tokens.h"

int
main (void)
{
  const char *		error_message;
  struct sexp_iterator	I[1];
  static const char *	sex = "(5:alpha4:beta5:gamma)";

  E(sexp_iterator_first(I, strlen(sex), (const uint8_t *)sex),
    "initialising the iterator");
  E(sexp_iterator_enter_list(I), "entering list");
  {
    print_atom(I);
    E(sexp_iterator_next(I), "advance to 2nd atom");
    print_atom(I);
    E(sexp_iterator_next(I), "advance to 3rd atom");
    print_atom(I);
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
