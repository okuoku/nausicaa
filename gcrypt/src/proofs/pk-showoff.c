/* pk-showoff.c */

#include <stdlib.h>
#include <stdio.h>
#include <gcrypt.h>

int
main (void)
{
  gcry_sexp_t	key_parms_sexp, key_pair_sexp;
  gcry_sexp_t	public_key_sexp, private_key_sexp;
  gcry_sexp_t	data_sexp, enc_sexp, dec_sexp;
  gcry_sexp_t	signature_sexp;
  gcry_mpi_t	data;
  int rc;

  rc = gcry_sexp_new(&key_parms_sexp, "(genkey (rsa (nbits 4:1024)) (transient-key))", 0, 1);
  if (rc) goto error;

  rc = gcry_pk_genkey(&key_pair_sexp, key_parms_sexp);
  if (rc) goto error;

  gcry_sexp_release (key_parms_sexp);

  public_key_sexp = gcry_sexp_find_token (key_pair_sexp, "public-key", 0);
  if (! public_key_sexp) goto error;

  private_key_sexp = gcry_sexp_find_token (key_pair_sexp, "private-key", 0);
  if (! private_key_sexp) goto error;

  gcry_sexp_release (key_pair_sexp);

  data = gcry_mpi_new(50);
  gcry_mpi_set_ui(data, 123);

  rc = gcry_sexp_build(&data_sexp, NULL, "(data (flags raw) (value %m))", data);
  if (rc) goto error;

  rc = gcry_pk_encrypt(&enc_sexp, data_sexp, public_key_sexp);
  if (rc) goto error;

  rc = gcry_pk_decrypt(&dec_sexp, enc_sexp, private_key_sexp);
  if (rc) goto error;

  rc = gcry_pk_sign(&signature_sexp, data_sexp, private_key_sexp);
  if (rc) goto error;

  rc = gcry_pk_verify(signature_sexp, data_sexp, public_key_sexp);
  if ((0 != rc) && (GPG_ERR_BAD_SIGNATURE != rc))
    goto error;

  fprintf(stderr, "correct signature? %s\n", (0 == rc)? "yes" : "no");

  gcry_mpi_release(data);
  gcry_sexp_release(enc_sexp);
  gcry_sexp_release(dec_sexp);
  gcry_sexp_release(signature_sexp);
  gcry_sexp_release(private_key_sexp);
  gcry_sexp_release(public_key_sexp);
  exit(EXIT_SUCCESS);

 error:
  fprintf(stderr, "error %s\n", gcry_strerror(rc));
  exit(EXIT_FAILURE);
}

/* end of file */
