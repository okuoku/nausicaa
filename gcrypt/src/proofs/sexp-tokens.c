/* sexp-tokens.c --

GCRYPT		= libgcrypt-config
CPPFLAGS	= $(shell $(GCRYPT) --cflags)
LIBS		= $(shell $(GCRYPT) --libs)
RM		= rm -f
CC		= gcc -std=c99
CFLAGS		= -Wall
TARGETS		= sexp-print sexp-tokens
.PHONY: all clean
all: $(TARGETS)
clean:
	$(RM) $(TARGETS)
$(TARGETS): % : %.c
	$(CC) $(CPPFLAGS) $(LIBS) $(CFLAGS) -o $(@) $(<)
 */

#include <stdio.h>
#include <stdlib.h>
#include <gcrypt.h>

#define GOK(EXP)		\
  do { e = EXP; if (e) goto error; } while (0)

static void
sexp_tokens (gcry_sexp_t sex)
{
  size_t	nitems = gcry_sexp_length(sex);
  size_t	len;
  const char *	ptr;
  gcry_sexp_t	sub;
  for (size_t i=0; i<nitems; ++i) {
    ptr = gcry_sexp_nth_data(sex, i, &len);
    if (NULL == ptr) {
      sub = gcry_sexp_nth(sex, i);
      fprintf(stderr, "(\n");
      sexp_tokens(sub);
      fprintf(stderr, ")\n");
      gcry_sexp_release(sub);
    } else {
      char		buffer[1+len];
      char *		tail;
      unsigned long	n;
      memcpy(buffer, ptr, len);
      buffer[len] = '\0';
      n = strtoul(buffer, &tail, 0);
      if (buffer < tail) {
	fprintf(stderr, "ulong: %lu\n", n);
      } else {
	if ('\0' == ptr[0]) {
	  gcry_mpi_t	n;
	  n = gcry_sexp_nth_mpi(sex, i, GCRYMPI_FMT_USG);
	  if (NULL != n) {
	    fprintf(stderr, "mpi\n");
	  } else {
	    fprintf(stderr, "invalid MPI\n");
	    exit(EXIT_FAILURE);
	  }
	} else {
	  fprintf(stderr, "tag: ");
	  fwrite(ptr, len, 1, stderr);
	  fprintf(stderr, "\n");
	}
      }
    }
  }
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
  if (0)
    sexp_tokens(parms);
  gcry_sexp_release(parms);
  if (1)
    sexp_tokens(key_pair);
  gcry_sexp_release(key_pair);
  exit(EXIT_SUCCESS);
 error:
  fprintf(stderr, "error: %s\n", gcry_strerror(e));
  exit(EXIT_FAILURE);
}

/* end of file */
