/* add.c --

   gcc -std=c99 -Wall				\
     -L/usr/local/lib -lmpfr -lmpc -lmpfrcx	\
     -o add add.c
*/

#include <stdio.h>
#include <stdlib.h>
#include <mpfr.h>
#include <mpc.h>
#include <mpfrcx.h>

int
main (int argc, const char *const argv[])
{
  mpfrx_t	a, b, c;
  mpfrx_init(a, 5, 50);
  mpfrx_init(b, 5, 50);
  mpfrx_init(c, 5, 50);
  {
    mpfr_set_d(a->coeff[0], 1.2, GMP_RNDN);
    mpfr_set_d(a->coeff[1], 3.4, GMP_RNDN);
    mpfr_set_d(a->coeff[2], 5.6, GMP_RNDN);
    mpfr_set_d(a->coeff[3], 7.8, GMP_RNDN);
    mpfr_set_d(a->coeff[4], 9.0, GMP_RNDN);
    mpfr_set_d(b->coeff[0], 1., GMP_RNDN);
    mpfr_set_d(b->coeff[1], 3., GMP_RNDN);
    mpfr_set_d(b->coeff[2], 5., GMP_RNDN);
    mpfr_set_d(b->coeff[3], 7., GMP_RNDN);
    mpfr_set_d(b->coeff[4], 9., GMP_RNDN);
    mpfrx_add(c, a, b);
    for (int i=0; i<5; ++i)
      mpfr_printf("%Rf\n", a->coeff[i]);
  }
  mpfrx_clear(a);
  mpfrx_clear(b);
  mpfrx_clear(c);
  exit(EXIT_SUCCESS);
}

/* end of file */
