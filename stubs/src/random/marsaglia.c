/*
  Part of: Nausicaa/Stubs
  Contents: G. Marsaglia's pseudo-random numbers generators
  Date: Sat Jun 27, 2009

  Abstract

	See the abstract of the header file "marsaglia.h".

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


int
nausicaa_marsaglia_context_size (void)
{
  return sizeof(marsaglia_context_t);
}
int
nausicaa_marsaglia_seed_size (void)
{
  return (256 + 6); /* we count only: z, w, jsr, jcong, a, b. */
}
void
nausicaa_marsaglia_init (marsaglia_context_t * ctx, uint32_t * seed)
{
  ctx->x	= 0;
  ctx->y	= 0;
  ctx->bro	= 0;
  ctx->c	= 0;
  ctx->z	= seed[0];
  ctx->w	= seed[1];
  ctx->jsr	= seed[2];
  ctx->jcong	= seed[3];
  ctx->a	= seed[4];
  ctx->b	= seed[5];
  for (int i=0; i<256; ++i)
    ctx->t[i] = seed[i];
}
uint32_t nausicaa_marsaglia_random_cong	(marsaglia_context_t * ctx) { return CONG; }
uint32_t nausicaa_marsaglia_random_fib	(marsaglia_context_t * ctx) { return FIB; }
uint32_t nausicaa_marsaglia_random_kiss	(marsaglia_context_t * ctx) { return KISS; }
uint32_t nausicaa_marsaglia_random_lfib4(marsaglia_context_t * ctx) { return LFIB4; }
uint32_t nausicaa_marsaglia_random_shr3	(marsaglia_context_t * ctx) { return SHR3; }
uint32_t nausicaa_marsaglia_random_swb	(marsaglia_context_t * ctx) { return SWB; }

#define GET_ARRAY(NAME,OPERATOR)					\
									\
  void									\
  nausicaa_marsaglia_get_array_ ## NAME (marsaglia_context_t * ctx,	\
					 int len, uint32_t * ptr)	\
  {									\
    for (int i = 0; i<len; i++)						\
      ptr[i] = OPERATOR;						\
  }

GET_ARRAY(cong,CONG)
GET_ARRAY(fib,FIB)
GET_ARRAY(kiss,KISS)
GET_ARRAY(lfib4,LFIB4)
GET_ARRAY(shr3,SHR3)
GET_ARRAY(swb,SWB)

#define JUMPAHEAD(NAME,OPERATOR)					\
									\
  void									\
  nausicaa_marsaglia_jumpahead_ ## NAME (marsaglia_context_t *ctx,	\
					 int steps)			\
  {									\
    for (int i = 0; i<steps; i++)					\
      OPERATOR;								\
  }

JUMPAHEAD(cong,CONG)
JUMPAHEAD(fib,FIB)
JUMPAHEAD(kiss,KISS)
JUMPAHEAD(lfib4,LFIB4)
JUMPAHEAD(shr3,SHR3)
JUMPAHEAD(swb,SWB)

/* end of file */
