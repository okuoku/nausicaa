/* socket-options.c --

   Shows  that SO_SNDBUF and  SO_RCVBUF are  suggestions to  the system,
   rather than  strict orders.  On  i686-pc-linux-gnu and Glibc  2.7, we
   set a value and we get a different value.

   gcc -Wall -o socket-options socket-options.c
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>

int
main (int argc, const char *const argv[])
{
  int		fd;
  size_t	val = 1000;
  size_t	val1 = 0;
  socklen_t	len = sizeof(size_t);
  fd = socket(PF_LOCAL, SOCK_STREAM, 0);
  setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &val, sizeof(size_t));
  getsockopt(fd, SOL_SOCKET, SO_SNDBUF, &val1, &len);
  printf("%u %u\n", val, val1);
  exit(EXIT_SUCCESS);
}

/* end of file */
