/*
  Part of: Nausicaa/Stubs
  Contents: tests for G. Marsaglia's pseudo-random numbers generators
  Date: Sat Jun 27, 2009

  Abstract



  Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
  Copyright (c) 1999 George Marsaglia <geo@stat.fsu.edu>

  Modified for Nausicaa/Stubs by Marco Maggi.

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <marsaglia.h>
#include <stdio.h>
#include <stdlib.h>



static void
settable (marsaglia_context_t * ctx,
	  uint32_t i1,uint32_t i2,uint32_t i3,uint32_t i4,uint32_t i5, uint32_t i6)
{ /* Example procedure to set the table, using KISS: */
  ctx->x   = 0;
  ctx->y   = 0;
  ctx->bro = 0;
  ctx->c   = 0;
  ctx->z = i1, ctx->w = i2, ctx->jsr = i3; ctx->jcong = i4; ctx->a = i5; ctx->b = i6;
  for(int i=0; i<256; ++i=)
    t[i]=KISS;
}

int
main (void)
{ /* This is a test main program.  It should compile and print 7 0's. */
  static const marsaglia_context_t ctx = {
    .z		= 362436069,
    .w		= 521288629,
    .jsr	= 123456789,
    .jcong	= 380116160,
    .a		= 224466889,
    .b		= 7584631
  };
  int		i, times = 1000001;
  uint32_t	k;

  settable(12345, 65435, 34221, 12345, 9983651, 95746118);

  for(i=1; i<times; i++) k = LFIB4;
  printf("%u\n", k - (uint32_t)1064612766);

  for(i=1; i<times; i++) k = SWB;
  printf("%u\n", k - (uint32_t)627749721);

  for(i=1; i<times; i++) k = KISS;
  printf("%u\n", k - (uint32_t)1372460312);

  for(i=1; i<times; i++) k = CONG;
  printf("%u\n", k - (uint32_t)1529210297);

  for(i=1; i<times; i++) k = SHR3;
  printf("%u\n", k - (uint32_t)2642725982);

  for(i=1; i<times; i++) k = MWC;
  printf("%u\n", k - (uint32_t)904977562);

  for(i=1; i<times; i++) k = FIB;
  printf("%u\n", k - (uint32_t)3519793928);

  exit(EXIT_SUCCESS);
}

/* end of file */
