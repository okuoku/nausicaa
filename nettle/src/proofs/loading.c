/* loading.c */

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

int
main (void)
{
  void *	nettle;
  void *	hogweed;
  /* int		flags =  RTLD_NOW | RTLD_LOCAL; */
  int		flags =  RTLD_NOW | RTLD_GLOBAL;
  nettle  = dlopen("libnettle.so", flags);
  if (NULL == nettle) {
    fprintf(stderr, "opening nettle: %s\n", dlerror());
    goto error;
  }
  hogweed = dlopen("libhogweed.so", flags);
  if (NULL == hogweed) {
    fprintf(stderr, "opening hogweed: %s\n", dlerror());
    goto error;
  }
  exit(EXIT_SUCCESS);
 error:
  exit(EXIT_FAILURE);
}

/* end of file */
