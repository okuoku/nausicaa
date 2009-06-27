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


#ifndef NETTLE_KNUTH_LFIB_H_INCLUDED
#define NETTLE_KNUTH_LFIB_H_INCLUDED

#include <stdint.h>

#define _KNUTH_LFIB_KK 100

typedef struct knuth_lfib_context_t {
  uint32_t x[_KNUTH_LFIB_KK];
  unsigned index;
} knuth_lfib_context_t;

extern int nausicaa_knuth_lfib_context_size (void);
extern void nausicaa_knuth_lfib_init (knuth_lfib_context_t *ctx, int _seed);
/* Get a single number in the range [0, 2^30) */
extern uint32_t	nausicaa_knuth_lfib_random (knuth_lfib_context_t *ctx);
extern void nausicaa_knuth_lfib_get_array (knuth_lfib_context_t *ctx, int len, uint32_t * ptr);

#endif /* NETTLE_KNUTH_LFIB_H_INCLUDED */

/* end of file */
