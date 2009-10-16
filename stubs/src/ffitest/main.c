/*
   Part of: Nausicaa/Stubs
   Contents: foreign functions interface tests stub
   Date: Fri Oct 16, 2009

   Abstract

	This library  has the only  purpose or providing  test functions
	for Nausicaa's FFI interface.

   Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>

   This program is free software:  you can redistribute it and/or modify
   it under the terms of the  GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or (at
   your option) any later version.

   This program is  distributed in the hope that it  will be useful, but
   WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
   MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
   General Public License for more details.

   You should  have received  a copy of  the GNU General  Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** ------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */

typedef int callback_1_t (int a, int b, int c);

extern int nausicaa_ffitest_call_callback_1 (callback_1_t * cb, int a, int b, int c);


/** --------------------------------------------------------------------
 ** Callback tests.
 ** ----------------------------------------------------------------- */

int
nausicaa_ffitest_call_callback_1 (callback_1_t * cb, int a, int b, int c)
{
  return cb(a, b, c);
}


/* end of file */
