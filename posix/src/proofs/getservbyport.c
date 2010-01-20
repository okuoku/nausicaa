/* getservbyport.c --

   Print informations about a network service.
*/

#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <netinet/in.h>

int
main (void)
{
  struct servent *	info;
  const char *		alias;
  int			i;
  int			port = 25;

  printf("inspecting service port: %d\n", port);
  info = getservbyport(htons(port), "tcp");
  if (info) {
    printf("name: %s\n", info->s_name);
    for (i=0, alias = info->s_aliases[i]; alias; ++i, alias=info->s_aliases[i]) {
      printf("alias: %s\n", alias);
    }
    printf("port: %d\n", ntohs(info->s_port));
    printf("proto: %s\n", info->s_proto);
    exit(EXIT_SUCCESS);
  } else {
    fprintf(stderr, "service not found\n");
    exit(EXIT_FAILURE);
  }
}

/* end of file */
