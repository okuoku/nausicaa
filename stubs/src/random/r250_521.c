/*
   Part of: Nausicaa/Stubs
   Contents: R250/521 random number generator
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


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <string.h>
#include <r250_521.h>

#define R250_IA  (sizeof(uint32_t)*103)
#define R250_IB  (sizeof(uint32_t)*R250_LEN - R250_IA)
#define R521_IA  (sizeof(uint32_t)*168)
#define R521_IB  (sizeof(uint32_t)*R521_LEN - R521_IA)


int
nausicaa_r250_521_context_size (void)
{
  return sizeof(r250_521_context_t);
}
int
nausicaa_r250_521_seed_size (void)
{
  return R250_521_SEED_LEN;
}
void
nausicaa_r250_521_init (r250_521_context_t * ctx, uint32_t * seed)
{
  int		i = R521_LEN;
  uint32_t	mask1 = 1;
  uint32_t	mask2 = 0xFFFFFFFF;
  while (i-- > R250_LEN)
    ctx->r521_buffer[i] = seed[0];
  while (i-- > 31) {
    ctx->r250_buffer[i] = seed[1];
    ctx->r521_buffer[i] = seed[2];
  }
  /* Establish  linear independence of  the bit  columns by  setting the
     diagonal bits and clearing all bits above. */
  while (i-- > 0) {
    ctx->r250_buffer[i] = (seed[3] | mask1) & mask2;
    ctx->r521_buffer[i] = (seed[4] | mask1) & mask2;
    mask2 ^= mask1;
    mask1 >>= 1;
  }
  ctx->r250_buffer[0] = mask1;
  ctx->r521_buffer[0] = mask2;
  ctx->r250_index = 0;
  ctx->r521_index = 0;
}
uint32_t
nausicaa_r250_521_random (r250_521_context_t * ctx)
{
  /* I prescale the indices  by sizeof(uint32_t) to eliminate four shlwi
     instructions  in  the   compiled  code.   This  minor  optimization
     increased perf by about 12%.

     I  also  carefully  arrange  index increments  and  comparisons  to
     minimize  instructions.  gcc 3.3  seems a  bit weak  on instruction
     reordering. The  j1/j2 branches are  mispredicted, but nevertheless
     these optimizations increased perf by another 10%. */
  int		i1 = ctx->r250_index;
  int		i2 = ctx->r521_index;
  uint8_t *	b1 = (uint8_t*) ctx->r250_buffer;
  uint8_t *	b2 = (uint8_t*) ctx->r521_buffer;
  uint32_t *	tmp1;
  uint32_t *	tmp2;
  uint32_t	r, s;
  int		j1, j2;
  j1 = i1 - R250_IB;
  if (j1 < 0)
    j1 = i1 + R250_IA;
  j2 = i2 - R521_IB;
  if (j2 < 0)
    j2 = i2 + R521_IA;
  tmp1  = (uint32_t *)(b1 + i1);
  r     = (*(uint32_t *)(b1 + j1)) ^ (*tmp1);
  *tmp1 = r;
  tmp2  = (uint32_t *)(b2 + i2);
  s     = (*(uint32_t *)(b2 + j2)) ^ (*tmp2);
  *tmp2 = s;
  i1 = (i1 != sizeof(uint32_t)*(R250_LEN-1)) ? (i1 + sizeof(uint32_t)) : 0;
  ctx->r250_index = i1;
  i2 = (i2 != sizeof(uint32_t)*(R521_LEN-1)) ? (i2 + sizeof(uint32_t)) : 0;
  ctx->r521_index = i2;
  return r ^ s;
}
void
nausicaa_r250_521_get_array (r250_521_context_t * ctx, int len, uint32_t * ptr)
{
  for (int i = 0; i<len; i++)
    ptr[i] = nausicaa_r250_521_random(ctx);
}

/* end of file */
