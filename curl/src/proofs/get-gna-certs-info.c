/* get-gna-certs-info.c --

   Compile with:

   gcc -Wall -o proof proof.c $(curl-config --cflags) $(curl-config --libs)
*/

#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

#define COK(CALL)				\
  do {						\
    e = (CALL);					\
    if (CURLE_OK != e) {			\
      fprintf(stderr, curl_easy_strerror(e));	\
      exit(EXIT_FAILURE);			\
    }						\
  } while (0)

size_t
cb (void * buffer, size_t item_size, size_t item_number, void * custom)
{
  /* fwrite(buffer, item_size, item_number, stderr); */
  return (item_size * item_number);
}
int
main (int argc, const char *const argv[])
{
  CURL *	handle = curl_easy_init();
  struct curl_certinfo * certs = NULL;
  int		e;
  fprintf(stderr, "connecting to gna\n");
  COK(curl_easy_setopt(handle, CURLOPT_URL, "https://gna.org/"));
  COK(curl_easy_setopt(handle, CURLOPT_WRITEFUNCTION, cb));
  COK(curl_easy_setopt(handle, CURLOPT_WRITEDATA, NULL));
  COK(curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, 0));
  COK(curl_easy_setopt(handle, CURLOPT_CERTINFO, 1));
  COK(curl_easy_perform(handle));
  COK(curl_easy_getinfo(handle, CURLINFO_CERTINFO, &certs));
  fprintf(stderr, "num certs %d\n", certs->num_of_certs);
  exit(EXIT_SUCCESS);
}

/* end of file */
