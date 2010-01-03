/* sexp-print.c */

#include <stdio.h>
#include <stdlib.h>
#include <gcrypt.h>

#define GOK(EXP)		\
  do { e = EXP; if (e) goto error; } while (0)

void
doit (gcry_sexp_t sex, int mode)
{
  size_t	len = gcry_sexp_sprint(sex, mode, NULL, 0);
  char		buffer[len];
  gcry_sexp_sprint(sex, GCRYSEXP_FMT_CANON, buffer, len);
  /* We use "fwrite()"  because there may be zeros in  the buffer when a
     format other than ADVANCED is used. */
  fwrite(buffer, len, 1, stderr);
  /* fprintf(stderr, "%s\n", buffer); */
}
int
main (int argc, const char *const argv[])
{
  gcry_sexp_t	key_pair, parms;
  int		e;
  GOK(gcry_sexp_new(&parms, "(genkey (dsa (nbits 4:1024)(transient-key)))", 0, 1));
  fprintf(stderr, "generating keys..."); fflush(stderr);
  GOK(gcry_pk_genkey(&key_pair, parms));
  fprintf(stderr, " done\n");
  gcry_sexp_release(parms);
  fprintf(stderr, "printing:\n");
  /* doit(key_pair, GCRYSEXP_FMT_CANON); */
  /* doit(key_pair, GCRYSEXP_FMT_DEFAULT); */
  doit(key_pair, GCRYSEXP_FMT_ADVANCED);
  gcry_sexp_release(key_pair);
  exit(EXIT_SUCCESS);
 error:
  fprintf(stderr, "error: %s\n", gcry_strerror(e));
  exit(EXIT_FAILURE);
}

/* end of file */
