/* rsa-sign.c */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <nettle/md5.h>
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
  mpz_t				signature;

  rsa_private_key_init(&key);
  rsa_public_key_init(&pub);

  knuth_lfib_init(&lfib, 123);

  if (! rsa_generate_keypair(&pub, &key, &lfib, (nettle_random_func *) knuth_lfib_random,
			     NULL, progress, 1024, 50))
    goto error;

  rsa_private_key_prepare(&key);
  rsa_public_key_prepare(&pub);
  mpz_init(signature);

  {
    struct md5_ctx	ctx;
    md5_init(&ctx);
    md5_update(&ctx, 4, (uint8_t *)"ciao");
    rsa_md5_sign(&key, &ctx, signature);
  }
  {
    struct md5_ctx	ctx;
    md5_init(&ctx);
    md5_update(&ctx, 4, (uint8_t *)"ciao");
    if (1 == rsa_md5_verify(&pub, &ctx, signature))
      printf("ok\n");
    else
      printf("error\n");
  }

  mpz_clear(signature);
  rsa_public_key_clear(&pub);
  rsa_private_key_clear(&key);
  exit(EXIT_SUCCESS);

 error:
  exit(EXIT_FAILURE);
}

/* end of file */
