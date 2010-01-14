/* sexp-tokens.h */

#ifndef __SEXP_TOKENS_H
#  define __SEXP_TOKENS_H 1

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <nettle/sexp.h>

#define E(RESULT, MESSAGE)		\
					\
  do {					\
    if (! (RESULT)) {			\
      error_message = (MESSAGE);	\
      goto error;			\
    }					\
  } while (0);

#define newline()			\
					\
  printf("\n")

static void
print_atom (struct sexp_iterator * I)
{
  assert(SEXP_ATOM == I->type);
  printf("atom: ");
  fwrite(I->atom, I->atom_length, sizeof(char), stdout);
  newline();
}

#endif

/* end of file */
