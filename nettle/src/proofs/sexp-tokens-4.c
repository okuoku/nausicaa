/* sexp-tokens-4.c --

   Raw tokenisation for a sexp using key/value parsing.
*/

#include "sexp-tokens.h"

int
main (void)
{
  const char *			error_message;
  struct sexp_iterator		I[1], S[1];
  static const char *		sex = "((5:alpha4:beta5:gamma))";
#define KEYNUM		1
  static const char *const	keys[KEYNUM] = { "alpha" };

  E(sexp_iterator_first(I, strlen(sex), (const uint8_t *)sex),
    "initialising the iterator");

  E(sexp_iterator_enter_list(I), "entering list");
  for (int i=0; i<KEYNUM; ++i)
    S[i].type = SEXP_END;
  E(sexp_iterator_assoc(I, KEYNUM, (const uint8_t *const *)keys,
			S), "parsing keys/values");
  if (SEXP_END != S[0].type) {
    print_atom(S);
    E(sexp_iterator_next(S), "advance to 2rd atom");
    print_atom(S);
    E(sexp_iterator_next(S), "advance to subsexp end");
    assert(SEXP_END == S->type);
  }
  assert(SEXP_END == I->type);

  exit(EXIT_SUCCESS);
 error:
  fprintf(stderr, "error: %s\n", error_message);
  exit(EXIT_FAILURE);
}

/* end of file */
