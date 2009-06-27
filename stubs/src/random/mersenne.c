/*
   Part of: Nausicaa/Stubs
   Contents: Mersenne Twister random number generator
   Date: Mon Apr 24, 2006

   Abstract



   Copyright (c) 2006, 2009 Marco Maggi <marcomaggi@gna.org>
   Copyright (c) 1990-2005 Michael Brundage
   <http://www.qbrundage.com/michaelb/pubs/essays/random_number_generation>

   Modified for Guile-Random by Marco Maggi
   Modified for Nausicaa/Stubs by Marco Maggi

   CREATIVE COMMONS PUBLIC DOMAIN DEDICATION

   Copyright-Only  Dedication (based  on  United States  law) or  Public
   Domain Certification

   The person  or persons  who have associated  work with  this document
   (the "Dedicator" or "Certifier") hereby either (a) certifies that, to
   the best  of his knowledge, the  work of authorship  identified is in
   the public domain of the country from which the work is published, or
   (b) hereby  dedicates whatever copyright the dedicators  holds in the
   work  of  authorship identified  below  (the  "Work")  to the  public
   domain. A  certifier, moreover,  dedicates any copyright  interest he
   may have in the associated work, and for these purposes, is described
   as a "dedicator" below.

   A certifier has taken reasonable steps to verify the copyright status
   of this  work. Certifier recognizes  that his good faith  efforts may
   not shield him from liability if in fact the work certified is not in
   the public domain.

   Dedicator  makes this  dedication for  the benefit  of the  public at
   large   and  to   the  detriment   of  the   Dedicator's   heirs  and
   successors. Dedicator intends  this dedication to be an  overt act of
   relinquishment in  perpetuity of all present and  future rights under
   copyright law,  whether vested or contingent, in  the Work. Dedicator
   understands  that  such relinquishment  of  all  rights includes  the
   relinquishment  of all rights  to enforce  (by lawsuit  or otherwise)
   those copyrights in the Work.

   Dedicator recognizes that, once placed in the public domain, the Work
   may be  freely reproduced, distributed,  transmitted, used, modified,
   built  upon,  or  otherwise  exploited  by anyone  for  any  purpose,
   commercial or  non-commercial, and in  any way, including  by methods
   that have not yet been invented or conceived.
*/


/** ------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <string.h>
#include <mersenne.h>

#define MT_IA           397
#define MT_IB           (MERSENNE_BUFFER_LENGTH - MT_IA)
#define UPPER_MASK      0x80000000
#define LOWER_MASK      0x7FFFFFFF
#define MATRIX_A        0x9908B0DF
#define TWIST(b,i,j)    ((b)[i] & UPPER_MASK) | ((b)[j] & LOWER_MASK)
#define MAGIC(s)        (((s)&1)*MATRIX_A)


int
nausicaa_mersenne_context_size (void)
{
  return sizeof(mersenne_context_t);
}
int
nausicaa_mersenne_seed_size (void)
{
  return MERSENNE_BUFFER_LENGTH;
}
void
nausicaa_mersenne_init (mersenne_context_t * ctx, uint32_t * buffer)
{
  memcpy(&(ctx->buffer), buffer, sizeof(uint32_t)*MERSENNE_BUFFER_LENGTH);
  ctx->index  = 0;
}
uint32_t
nausicaa_mersenne_random (mersenne_context_t * ctx)
{
  uint32_t *	b   = ctx->buffer;
  int			idx = ctx->index;
  uint32_t		s;
  int			i;
  uint32_t		r;
  if (idx == MERSENNE_BUFFER_LENGTH * sizeof(uint32_t)) {
    idx = 0;
    i = 0;
    for (; i < MT_IB; i++) {
      s = TWIST(b, i, i+1);
      b[i] = b[i + MT_IA] ^ (s >> 1) ^ MAGIC(s);
    }
    for (; i < MERSENNE_BUFFER_LENGTH-1; i++) {
      s = TWIST(b, i, i+1);
      b[i] = b[i - MT_IB] ^ (s >> 1) ^ MAGIC(s);
    }
    s = TWIST(b, MERSENNE_BUFFER_LENGTH-1, 0);
    b[MERSENNE_BUFFER_LENGTH-1] = b[MT_IA-1] ^ (s >> 1) ^ MAGIC(s);
  }
  ctx->index = idx + sizeof(uint32_t);
  r = *(uint32_t *)((uint8_t *)b + idx);
  /* Note  by  Michael Brundage:  Matsumoto  and Nishimura  additionally
     confound the bits returned to  the caller but this doesn't increase
     the randomness, and slows down the generator by as much as 25%.  So
     I omit these operations here.

     Note  by  Marco  Maggi:  On   2009  CPUs  the  overhead  should  be
     sustainable, so I restored these operations.
  */
  r ^= (r >> 11);
  r ^= (r <<  7) & 0x9D2C5680;
  r ^= (r << 15) & 0xEFC60000;
  r ^= (r >> 18);
  return r;
}
void
nausicaa_mersenne_get_array (mersenne_context_t * ctx, int len, uint32_t * ptr)
{
  for (int i = 0; i<len; i++)
    ptr[i] = nausicaa_mersenne_random(ctx);
}


/* end of file */
