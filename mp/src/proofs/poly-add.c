/* add.c --

   gcc -std=c99 -Wall				\
     -L/usr/local/lib -lmpfr -lmpc -lmpfrcx	\
     -o poly-add poly-add.c
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
  mpfrx_init(a, 5, 50); a->deg = 4;
  mpfrx_init(b, 5, 50); b->deg = 4;
  mpfrx_init(c, 5, 50); c->deg = 4;
  {
    mpfr_set_d(a->coeff[0], 0.1, GMP_RNDN);
    mpfr_set_d(a->coeff[1], 0.2, GMP_RNDN);
    mpfr_set_d(a->coeff[2], 0.3, GMP_RNDN);
    mpfr_set_d(a->coeff[3], 0.4, GMP_RNDN);
    mpfr_set_d(a->coeff[4], 0.5, GMP_RNDN);

    mpfr_set_d(b->coeff[0], 1., GMP_RNDN);
    mpfr_set_d(b->coeff[1], 2., GMP_RNDN);
    mpfr_set_d(b->coeff[2], 3., GMP_RNDN);
    mpfr_set_d(b->coeff[3], 4., GMP_RNDN);
    mpfr_set_d(b->coeff[4], 5., GMP_RNDN);

    mpfrx_add(c, a, b);

    for (int i=0; i<5; ++i)
      mpfr_printf("%Rf\n", c->coeff[i]);

    mpfrx_out_str(stdout, 10, 5, c);
    printf("\n");
  }
  mpfrx_clear(a);
  mpfrx_clear(b);
  mpfrx_clear(c);
  exit(EXIT_SUCCESS);
}

/* end of file */
