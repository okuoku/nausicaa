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


void
nausicaa_marsaglia_init_cong (marsaglia_context_cong_t * ctx, uint32_t * seed)
{
  ctx->jcong	= seed[0];
}
int
nausicaa_marsaglia_seed_size_cong (void)
{
  return 1;
}
void
nausicaa_marsaglia_init_fib (marsaglia_context_fib_t * ctx, uint32_t * seed)
{
  ctx->a	= seed[0];
  ctx->b	= seed[1];
}
int
nausicaa_marsaglia_seed_size_fib (void)
{
  return 2;
}
void
nausicaa_marsaglia_init_kiss (marsaglia_context_kiss_t * ctx, uint32_t * seed)
{
  ctx->z	= seed[0];
  ctx->w	= seed[1];
  ctx->jsr	= seed[2];
  ctx->jcong	= seed[3];
}
int
nausicaa_marsaglia_seed_size_kiss (void)
{
  return 4;
}
void
nausicaa_marsaglia_init_lfib4 (marsaglia_context_lfib4_t * ctx, uint32_t * seed)
{
  ctx->c	= 0;
  for (int i=0; i<256; ++i)
    ctx->t[i] = seed[i];
}
int
nausicaa_marsaglia_seed_size_lfib4 (void)
{
  return 256; /* we count only t, c is a state */
}
void
nausicaa_marsaglia_init_shr3 (marsaglia_context_shr3_t * ctx, uint32_t * seed)
{
  ctx->jsr	= seed[0];
}
int
nausicaa_marsaglia_seed_size_shr3 (void)
{
  return 1;
}
void
nausicaa_marsaglia_init_swb (marsaglia_context_swb_t * ctx, uint32_t * seed)
{
  ctx->x	= 0;
  ctx->y	= 0;
  ctx->bro	= 0;
  ctx->c	= 0;
  for (int i=0; i<256; ++i)
    ctx->t[i] = seed[i];
}
int
nausicaa_marsaglia_seed_size_swb (void)
{
  return 256;
}



#define STUFF(NAME,OPERATOR)					\
								\
  int								\
  nausicaa_marsaglia_context_size_ ## NAME (void)		\
  {								\
    return sizeof(marsaglia_context_ ## NAME ## _t);		\
  }								\
  uint32_t							\
  nausicaa_marsaglia_random_ ## NAME				\
		(marsaglia_context_ ## NAME ## _t * ctx)	\
  {								\
    return OPERATOR;						\
  }								\
  void								\
  nausicaa_marsaglia_get_array_ ## NAME				\
		(marsaglia_context_ ## NAME ## _t * ctx,	\
		 int len, uint32_t * ptr)			\
  {								\
    for (int i = 0; i<len; i++)					\
      ptr[i] = OPERATOR;					\
  }								\
  void								\
  nausicaa_marsaglia_jumpahead_ ## NAME				\
		(marsaglia_context_ ## NAME ## _t *ctx,		\
		 int steps)					\
  {								\
    for (int i = 0; i<steps; i++)				\
      OPERATOR;							\
  }

STUFF(cong,CONG)
STUFF(fib,FIB)
STUFF(kiss,KISS)
STUFF(lfib4,LFIB4)
STUFF(shr3,SHR3)
STUFF(swb,SWB)

/* end of file */
