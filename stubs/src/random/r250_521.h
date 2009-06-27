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


#ifndef R250_521_H
#define R250_521_H

#include <stdlib.h>
#include <stdint.h>

#define R250_521_SEED_LEN	5
#define R250_LEN		250
#define R521_LEN		521

typedef struct r250_521_context_t {
  int			r250_index;
  int			r521_index;
  uint32_t		r250_buffer[R250_LEN];
  uint32_t		r521_buffer[R521_LEN];
} r250_521_context_t;

extern int nausicaa_r250_521_context_size (void);
extern int nausicaa_r250_521_seed_size (void);
/* The seed array must hold R250_521_SEED_LEN 32bits integers. */
extern void nausicaa_r250_521_init (r250_521_context_t * ctx, uint32_t * seed);
extern uint32_t	nausicaa_r250_521_random (r250_521_context_t * ctx);
extern void nausicaa_r250_521_get_array (r250_521_context_t * ctx, int len, uint32_t * ptr);

#endif /* R250_521_H */

/* end of file */
