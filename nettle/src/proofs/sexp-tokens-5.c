/* sexp-tokens-4.c --

   Raw tokenisation for a sexp using key/value parsing.
*/

#include "sexp-tokens.h"

int
main (void)
{
  const char *			error_message;
  struct sexp_iterator		I[1], S[2];
  static const char *		sex = "((5:alpha4:beta5:gamma)(5:delta5:theta))";
#define KEYNUM		2
  static const char *const	keys[KEYNUM] = { "alpha", "delta" };

  E(sexp_iterator_first(I, strlen(sex), (const uint8_t *)sex),
    "initialising the iterator");
  E(sexp_iterator_enter_list(I), "entering list");

  for (int i=0; i<KEYNUM; ++i)
    S[i].type = SEXP_END;
  E(sexp_iterator_assoc(I, KEYNUM, (const uint8_t *const *)keys,
			S), "parsing keys/values");
  if (SEXP_END != S[0].type) {
    print_atom(&S[0]);
    E(sexp_iterator_next(&S[0]), "advance to 2rd atom");
    print_atom(&S[0]);
    E(sexp_iterator_next(&S[0]), "advance to subsexp end");
    assert(SEXP_END == S[0].type);
  }
  if (SEXP_END != S[1].type) {
    print_atom(&S[1]);
    E(sexp_iterator_next(&S[1]), "advance to subsexp end");
    assert(SEXP_END == S[1].type);
  }
  assert(SEXP_END == I->type);

  exit(EXIT_SUCCESS);
 error:
  fprintf(stderr, "error: %s\n", error_message);
  exit(EXIT_FAILURE);
}

/* end of file */
