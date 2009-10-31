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
#include <stdint.h>
#include <errno.h>

#ifdef __GNUC__
#  define FFI_UNUSED		__attribute__((unused))
#else
#  define FFI_UNUSED		/* empty */
#  define __attribute__(...)	/* empty */
#endif


/** --------------------------------------------------------------------
 ** Prototypes.
 ** ----------------------------------------------------------------- */

extern int8_t		  nausicaa_ffitest_callout_int8		(int, int8_t, int);
extern int16_t		  nausicaa_ffitest_callout_int16	(int, int16_t, int);
extern int32_t		  nausicaa_ffitest_callout_int32	(int, int32_t, int);
extern int64_t		  nausicaa_ffitest_callout_int64	(int, int64_t, int);
extern uint8_t		  nausicaa_ffitest_callout_uint8	(int, uint8_t, int);
extern uint16_t		  nausicaa_ffitest_callout_uint16	(int, uint16_t, int);
extern uint32_t		  nausicaa_ffitest_callout_uint32	(int, uint32_t, int);
extern uint64_t		  nausicaa_ffitest_callout_uint64	(int, uint64_t, int);

extern char		  nausicaa_ffitest_callout_char		(int, char, int);
extern unsigned char	  nausicaa_ffitest_callout_uchar	(int, unsigned char, int);
extern short int	  nausicaa_ffitest_callout_short	(int, short int, int);
extern unsigned short int nausicaa_ffitest_callout_ushort	(int, unsigned short int, int);
extern int		  nausicaa_ffitest_callout_int		(int, int, int);
extern unsigned int	  nausicaa_ffitest_callout_uint		(int, unsigned int, int);
extern long		  nausicaa_ffitest_callout_long		(int, long, int);
extern unsigned long	  nausicaa_ffitest_callout_ulong	(int, unsigned long, int);
extern long long	  nausicaa_ffitest_callout_llong	(int, long long, int);
extern unsigned long long nausicaa_ffitest_callout_ullong	(int, unsigned long long, int);

extern float		  nausicaa_ffitest_callout_float	(int, float, int);
extern double		  nausicaa_ffitest_callout_double	(int, double, int);
extern void *		  nausicaa_ffitest_callout_pointer	(int, void *, int);

/* ------------------------------------------------------------------ */

typedef int8_t		  callback_int8_t			(int a, int8_t   b, int c);
typedef int16_t		  callback_int16_t			(int a, int16_t  b, int c);
typedef int32_t		  callback_int32_t			(int a, int32_t  b, int c);
typedef int64_t		  callback_int64_t			(int a, int64_t  b, int c);
typedef uint8_t		  callback_uint8_t			(int a, uint8_t  b, int c);
typedef uint16_t	  callback_uint16_t			(int a, uint16_t b, int c);
typedef uint32_t	  callback_uint32_t			(int a, uint32_t b, int c);
typedef uint64_t	  callback_uint64_t			(int a, uint64_t b, int c);

typedef char		  callback_char_t			(int a, char b, int c);
typedef unsigned char	  callback_uchar_t			(int a, unsigned char b, int c);
typedef int		  callback_int_t			(int a, int b, int c);
typedef unsigned int	  callback_uint_t			(int a, unsigned int b, int c);
typedef short int	  callback_short_t			(int a, short b, int c);
typedef unsigned short	  callback_ushort_t			(int a, unsigned short b, int c);
typedef long int	  callback_long_t			(int a, long b, int c);
typedef unsigned long	  callback_ulong_t			(int a, unsigned long b, int c);
typedef long long int	  callback_llong_t			(int a, long long b, int c);
typedef unsigned long long callback_ullong_t			(int a, unsigned long long b, int c);

typedef float		  callback_float_t			(int a, float b, int c);
typedef double		  callback_double_t			(int a, double b, int c);
typedef void *		  callback_pointer_t			(int a, void * b, int c);


extern int8_t  nausicaa_ffitest_callback_int8  (callback_int8_t  *, int, int8_t, int);
extern int16_t nausicaa_ffitest_callback_int16 (callback_int16_t *, int, int16_t, int);
extern int32_t nausicaa_ffitest_callback_int32 (callback_int32_t *, int, int32_t, int);
extern int64_t nausicaa_ffitest_callback_int64 (callback_int64_t *, int, int64_t, int);
extern uint8_t  nausicaa_ffitest_callback_uint8  (callback_uint8_t  *, int, uint8_t, int);
extern uint16_t nausicaa_ffitest_callback_uint16 (callback_uint16_t *, int, uint16_t, int);
extern uint32_t nausicaa_ffitest_callback_uint32 (callback_uint32_t *, int, uint32_t, int);
extern uint64_t nausicaa_ffitest_callback_uint64 (callback_uint64_t *, int, uint64_t, int);

extern char nausicaa_ffitest_callback_char (callback_char_t *, int, char, int);
extern unsigned char nausicaa_ffitest_callback_uchar (callback_uchar_t *, int, unsigned char, int);
extern short nausicaa_ffitest_callback_short (callback_short_t *, int, short, int);
extern unsigned short nausicaa_ffitest_callback_ushort (callback_ushort_t *, int, unsigned short, int);
extern int nausicaa_ffitest_callback_int (callback_int_t * , int, int, int);
extern unsigned nausicaa_ffitest_callback_uint (callback_int_t *, int, unsigned, int);
extern long nausicaa_ffitest_callback_long (callback_long_t *, int, long, int);
extern unsigned long nausicaa_ffitest_callback_ulong (callback_ulong_t *, int, unsigned long, int);
extern long long nausicaa_ffitest_callback_llong (callback_llong_t *, int, long long, int);
extern unsigned long long nausicaa_ffitest_callback_ullong (callback_ullong_t *, int,
							    unsigned long long, int);

extern float nausicaa_ffitest_callback_float (callback_float_t *, int, float, int);
extern double nausicaa_ffitest_callback_double (callback_double_t *, int, double, int);
extern void * nausicaa_ffitest_callback_pointer (callback_pointer_t *, int, void *, int);


/** --------------------------------------------------------------------
 ** Callouts.
 ** ----------------------------------------------------------------- */

int8_t
nausicaa_ffitest_callout_int8 (int a FFI_UNUSED, int8_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
int16_t
nausicaa_ffitest_callout_int16 (int a FFI_UNUSED, int16_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
int32_t
nausicaa_ffitest_callout_int32 (int a FFI_UNUSED, int32_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
int64_t
nausicaa_ffitest_callout_int64 (int a FFI_UNUSED, int64_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
uint8_t
nausicaa_ffitest_callout_uint8 (int a FFI_UNUSED, uint8_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
uint16_t
nausicaa_ffitest_callout_uint16 (int a FFI_UNUSED, uint16_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
uint32_t
nausicaa_ffitest_callout_uint32 (int a FFI_UNUSED, uint32_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
uint64_t
nausicaa_ffitest_callout_uint64 (int a FFI_UNUSED, uint64_t b, int c FFI_UNUSED)
{
  errno = a; return b;
}
char
nausicaa_ffitest_callout_char (int a FFI_UNUSED, char b, int c FFI_UNUSED)
{
  errno = a; return b;
}
unsigned char
nausicaa_ffitest_callout_uchar (int a FFI_UNUSED, unsigned char b, int c FFI_UNUSED)
{
  errno = a; return b;
}
short int
nausicaa_ffitest_callout_short (int a FFI_UNUSED, short int b, int c FFI_UNUSED)
{
  errno = a; return b;
}
unsigned short int
nausicaa_ffitest_callout_ushort (int a FFI_UNUSED, unsigned short int b, int c FFI_UNUSED)
{
  errno = a; return b;
}
int
nausicaa_ffitest_callout_int (int a FFI_UNUSED, int b, int c FFI_UNUSED)
{
  errno = a; return b;
}
unsigned int
nausicaa_ffitest_callout_uint (int a FFI_UNUSED, unsigned int b, int c FFI_UNUSED)
{
  errno = a; return b;
}
long
nausicaa_ffitest_callout_long (int a FFI_UNUSED, long b, int c FFI_UNUSED)
{
  errno = a; return b;
}
unsigned long
nausicaa_ffitest_callout_ulong (int a FFI_UNUSED, unsigned long b, int c FFI_UNUSED)
{
  errno = a; return b;
}
long long
nausicaa_ffitest_callout_llong (int a FFI_UNUSED, long long b, int c FFI_UNUSED)
{
  errno = a; return b;
}
unsigned long long
nausicaa_ffitest_callout_ullong (int a FFI_UNUSED, unsigned long long b, int c FFI_UNUSED)
{
  errno = a; return b;
}
float
nausicaa_ffitest_callout_float (int a FFI_UNUSED, float b, int c FFI_UNUSED)
{
  errno = a; return b;
}
double
nausicaa_ffitest_callout_double (int a FFI_UNUSED, double b, int c FFI_UNUSED)
{
  errno = a; return b;
}
void *
nausicaa_ffitest_callout_pointer (int a FFI_UNUSED, void * b, int c FFI_UNUSED)
{
  errno = a; return b;
}


/** --------------------------------------------------------------------
 ** Callback tests.
 ** ----------------------------------------------------------------- */

int8_t
nausicaa_ffitest_callback_int8  (callback_int8_t  * cb, int a, int8_t b, int c)
{
  int8_t	result = cb(a, b, c);
  /* fprintf(stderr, "%s %d %d\n", __func__, a, result); */
  return result;
}
int16_t
nausicaa_ffitest_callback_int16 (callback_int16_t * cb, int a, int16_t b, int c)
{
  return cb(a, b, c);
}
int32_t
nausicaa_ffitest_callback_int32 (callback_int32_t * cb, int a, int32_t b, int c)
{
  return cb(a, b, c);
}
int64_t
nausicaa_ffitest_callback_int64 (callback_int64_t * cb, int a, int64_t b, int c)
{
  return cb(a, b, c);
}
uint8_t
nausicaa_ffitest_callback_uint8  (callback_uint8_t  * cb, int a, uint8_t b, int c)
{
  return cb(a, b, c);
}
uint16_t
nausicaa_ffitest_callback_uint16 (callback_uint16_t * cb, int a, uint16_t b, int c)
{
  return cb(a, b, c);
}
uint32_t
nausicaa_ffitest_callback_uint32 (callback_uint32_t * cb, int a, uint32_t b, int c)
{
  return cb(a, b, c);
}
uint64_t
nausicaa_ffitest_callback_uint64 (callback_uint64_t * cb, int a, uint64_t b, int c)
{
  return cb(a, b, c);
}

char
nausicaa_ffitest_callback_char (callback_char_t * cb, int a, char b, int c)
{
  return cb(a, b, c);
}
unsigned char
nausicaa_ffitest_callback_uchar (callback_uchar_t * cb, int a, unsigned char b, int c)
{
  return cb(a, b, c);
}
short
nausicaa_ffitest_callback_short (callback_short_t * cb, int a, short b, int c)
{
  return cb(a, b, c);
}
unsigned short
nausicaa_ffitest_callback_ushort (callback_ushort_t * cb, int a, unsigned short b, int c)
{
  return cb(a, b, c);
}
int
nausicaa_ffitest_callback_int (callback_int_t * cb, int a, int b, int c)
{
  return cb(a, b, c);
}
unsigned
nausicaa_ffitest_callback_uint (callback_int_t * cb, int a, unsigned b, int c)
{
  return cb(a, b, c);
}
long
nausicaa_ffitest_callback_long (callback_long_t * cb, int a, long b, int c)
{
  return cb(a, b, c);
}
unsigned long
nausicaa_ffitest_callback_ulong (callback_ulong_t * cb, int a, unsigned long b, int c)
{
  return cb(a, b, c);
}
long long
nausicaa_ffitest_callback_llong (callback_llong_t * cb, int a, long long b, int c)
{
  return cb(a, b, c);
}
unsigned long long
nausicaa_ffitest_callback_ullong (callback_ullong_t * cb, int a, unsigned long long b, int c)
{
  return cb(a, b, c);
}

float
nausicaa_ffitest_callback_float (callback_float_t * cb, int a, float b, int c)
{
  return cb(a, b, c);
}
double
nausicaa_ffitest_callback_double (callback_double_t * cb, int a, double b, int c)
{
  return cb(a, b, c);
}
void *
nausicaa_ffitest_callback_pointer (callback_pointer_t * cb, int a, void * b, int c)
{
  return cb(a, b, c);
}


/* end of file */
