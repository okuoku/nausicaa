/* rsa-keygen.c */

#include <stdio.h>
#include <stdlib.h>
#include <nettle/rsa.h>
#include <nettle/knuth-lfib.h>

static void
progress (void * ctx, int c)
{
  fputc(c, stderr);
}

int
main (void)
{
  struct rsa_public_key		pub;
  struct rsa_private_key	key;
  struct knuth_lfib_ctx		lfib;

  rsa_private_key_init(&key);
  rsa_public_key_init(&pub);

  knuth_lfib_init(&lfib, 123);

  if (! rsa_generate_keypair(&pub, &key, &lfib, (nettle_random_func *) knuth_lfib_random,
			     NULL, progress, 1024, 50))
    goto error;

  rsa_private_key_prepare(&key);
  rsa_public_key_prepare(&pub);

  rsa_public_key_clear(&pub);
  rsa_private_key_clear(&key);
  exit(EXIT_SUCCESS);

 error:
  exit(EXIT_FAILURE);
}

/* end of file */
