/* conv.c

   Compile with:

   gcc -Wall -o conv conv.c -liconv

*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <iconv.h>

#define LEN		64

int
main (int argc, const char *const argv[])
{
  iconv_t	ctx;
  char *	input = "mamma";
  char	 	middle[LEN];
  char		output[LEN];
  char *	in  = input;
  char *	mid = middle;
  char *	out = output;
  size_t	input_len  = strlen(input);
  size_t	middle_len = LEN;
  size_t	output_len = LEN;
  int		e;

  ctx = iconv_open("UTF-16", "US-ASCII");
  e = iconv(ctx, &in, &input_len, &mid, &middle_len);
  if (-1 == e) {
    perror(strerror(errno));
  }
  e = iconv(ctx, NULL, NULL, &mid, &middle_len);
  if (-1 == e) {
    perror(strerror(errno));
  }
  iconv_close(ctx);

  printf("<");
  fwrite(middle, LEN - middle_len, 1, stdout);
  printf(">\n");

  mid        = middle;
  middle_len = LEN - middle_len;
  ctx = iconv_open("UTF-8", "UTF-16");
  iconv(ctx, &mid, &middle_len, &out, &output_len);
  iconv(ctx, NULL, NULL, &out, &output_len);
  iconv_close(ctx);

  printf("<%s>\n", output);
  exit(EXIT_SUCCESS);
}

/* end of file*/
