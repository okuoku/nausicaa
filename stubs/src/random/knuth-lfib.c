/* nettle, low-level cryptographics library

   Abstract
   --------

     	A "lagged  fibonacci" pseudorandomness generator.   Described in
 	Knuth, TAOCP, 3.6.

	Includes code copied verbatim from Knuth's TAOCP.

	NOTE: This generator  is totally inappropriate for cryptographic
 	applications.   It is  useful for  generating  deterministic but
 	random-looking test data, and is used by the Nettle testsuite.

   Copyright (C) 2006, 2009 Marco Maggi <marcomagg@gna.org>
   Copyright (C) 2002 Niels Möller

   Modified for inclusion in Guile-Random by Marco Maggi
   Modified for inclusion in Nausicaa/Stubs by Marco Maggi

   The nettle library  is free software; you can  redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License as
   published by the Free Software  Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   The nettle library is distributed in the hope that it will be useful,
   but  WITHOUT  ANY WARRANTY;  without  even  the  implied warranty  of
   MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
   Lesser General Public License for more details.

   You  should have received  a copy  of the  GNU Lesser  General Public
   License along with the nettle  library; see the file COPYING.LIB.  If
   not, write to  the Free Software Foundation, Inc.,  59 Temple Place -
   Suite 330, Boston, MA 02111-1307, USA.
*/



#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "knuth-lfib.h"

#define WRITE_UINT24(p, i)			\
do {						\
  (p)[0] = ((i) >> 16) & 0xff;			\
  (p)[1] = ((i) >> 8) & 0xff;			\
  (p)[2] = (i) & 0xff;				\
} while(0)
#define WRITE_UINT16(p, i)			\
do {						\
  (p)[0] = ((i) >> 8) & 0xff;			\
  (p)[1] = (i) & 0xff;				\
} while(0)


#define KK _KNUTH_LFIB_KK
#define LL 37
#define MM (1UL << 30)
#define TT 70



int
nausicaa_knuth_lfib_context_size (void)
{
  return sizeof(knuth_lfib_context_t);
}
int
nausicaa_knuth_lfib_seed_size (void)
{
  return 1;
}
void
nausicaa_knuth_lfib_init (knuth_lfib_context_t * ctx, uint32_t * _seed)
{
  int		t, j;
  uint32_t	x[2*KK - 1];
  uint32_t	seed = _seed[0];
  uint32_t	ss   = (seed + 2) & (MM-2);
  for (j = 0; j<KK; j++) {
    x[j] = ss;
    ss <<= 1;
    if (ss >= MM) ss -= (MM-2);
  }
  for (;j< 2*KK-1; j++)
    x[j] = 0;
  x[1]++;
  ss = seed & (MM-1);
  for (t = TT-1; t; ) {
    for (j = KK-1; j>0; j--)
      x[j+j] = x[j];
    for (j = 2*KK-2; j > KK-LL; j-= 2)
      x[2*KK-1-j] = x[j] & ~1;
    for (j = 2*KK-2; j>=KK; j--)
      if (x[j] & 1) {
	x[j-(KK-LL)] = (x[j - (KK-LL)] - x[j]) & (MM-1);
	x[j-KK] = (x[j-KK] - x[j]) & (MM-1);
      }
    if (ss & 1) {
      for (j=KK; j>0; j--)
	x[j] = x[j-1];
      x[0] = x[KK];
      if (x[KK] & 1)
	x[LL] = (x[LL] - x[KK]) & (MM-1);
    }
    if (ss)
      ss >>= 1;
    else
      t--;
  }
  for (j=0; j<LL; j++)
    ctx->x[j+KK-LL] = x[j];
  for (; j<KK; j++)
    ctx->x[j-LL] = x[j];
  ctx->index = 0;
}
uint32_t
nausicaa_knuth_lfib_random (knuth_lfib_context_t * ctx)
/* Get's a single number in the range 0 ... 2^30-1 */
{
  uint32_t	value;
  assert(ctx->index < KK);
  value = ctx->x[ctx->index];
  ctx->x[ctx->index] -= ctx->x[(ctx->index + KK - LL) % KK];
  ctx->x[ctx->index] &= (MM-1);
  ctx->index = (ctx->index + 1) % KK;
  return value;
}
void
nausicaa_knuth_lfib_get_array(knuth_lfib_context_t * ctx, int n, uint32_t *a)
{
  for (int i = 0; i<n; i++)
    a[i] = nausicaa_knuth_lfib_random(ctx);
}
void
nausicaa_knuth_lfib_jumpahead (knuth_lfib_context_t *ctx, int steps)
{
  for (int i = 0; i<steps; i++)
    nausicaa_knuth_lfib_random(ctx);
}

/* end of file */
