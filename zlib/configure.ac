dnl
dnl Part of: Nausicaa/Zlib
dnl Contents: configuration template
dnl Date: Sun Dec  7, 2008
dnl Time-stamp: <2008-12-08 13:48:48 marco>
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
dnl
dnl This  program  is free  software:  you  can redistribute  it
dnl and/or modify it  under the terms of the  GNU General Public
dnl License as published by the Free Software Foundation, either
dnl version  3 of  the License,  or (at  your option)  any later
dnl version.
dnl
dnl This  program is  distributed in  the hope  that it  will be
dnl useful, but  WITHOUT ANY WARRANTY; without  even the implied
dnl warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
dnl PURPOSE.   See  the  GNU  General Public  License  for  more
dnl details.
dnl
dnl You should  have received a  copy of the GNU  General Public
dnl License   along   with    this   program.    If   not,   see
dnl <http://www.gnu.org/licenses/>.
dnl

dnl page
dnl --------------------------------------------------------------------
dnl Setup.
dnl --------------------------------------------------------------------

AC_INIT([nausicaa-zlib],[0.1d0])
AC_PREREQ(2.60)
AC_CONFIG_SRCDIR([libraries/compile-all.sps])
NAUSICAA_BEGIN
NAUSICAA_IKARUS_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_YPSILON_CHECK_LIBRARY([URIEL_FFI],[(uriel ffi)])
NAUSICAA_SYSTEM_SETUP
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([zlib.h],,[AC_MSG_FAILURE([cannot find libz library],[2])])
AC_CHECK_LIB([z],[deflate],,[AC_MSG_FAILURE([cannot find libz library],[2])])
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Zlib header inspection.
dnl --------------------------------------------------------------------

NAUSICAA_SIZEOF_TEST([ZSTREAM],[z_stream],,[#include <zlib.h>])

AC_DEFUN([NAU_ZLIB_STREAM_FIELD],[
NAUSICAA_INSPECT_FIELD_TYPE([$1],[z_stream],[$2],[$3],,[#include <zlib.h>])
])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_NEXT_IN],[next_in],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_AVAIL_IN],[avail_in],[unsigned-integer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_TOTAL_IN],[total_in],[unsigned-integer])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_NEXT_OUT],[next_out],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_AVAIL_OUT],[avail_out],[unsigned-integer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_TOTAL_OUT],[total_out],[unsigned-integer])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_MSG],[msg],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_DATA_TYPE],[data_type],[signed-integer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_ADLER],[adler],[signed-integer])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_ZALLOC],[zalloc],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_ZFREE],[zfree],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_OPAQUE],[opaque],[pointer])


dnl page
dnl --------------------------------------------------------------------
dnl Constants inspection.
dnl --------------------------------------------------------------------

AC_DEFUN([NAU_ZLIB_VALUEOF],
   [NAUSICAA_VALUEOF_TEST([$1],[$1],,[#include <zlib.h>])])


NAU_ZLIB_VALUEOF([ZLIB_VERNUM])

NAU_ZLIB_VALUEOF([Z_NO_FLUSH])
NAU_ZLIB_VALUEOF([Z_PARTIAL_FLUSH])
NAU_ZLIB_VALUEOF([Z_SYNC_FLUSH])
NAU_ZLIB_VALUEOF([Z_FULL_FLUSH])
NAU_ZLIB_VALUEOF([Z_FINISH])
NAU_ZLIB_VALUEOF([Z_BLOCK])

NAU_ZLIB_VALUEOF([Z_OK])
NAU_ZLIB_VALUEOF([Z_STREAM_END])
NAU_ZLIB_VALUEOF([Z_NEED_DICT])
NAU_ZLIB_VALUEOF([Z_ERRNO])
NAU_ZLIB_VALUEOF([Z_STREAM_ERROR])
NAU_ZLIB_VALUEOF([Z_DATA_ERROR])
NAU_ZLIB_VALUEOF([Z_MEM_ERROR])
NAU_ZLIB_VALUEOF([Z_BUF_ERROR])
NAU_ZLIB_VALUEOF([Z_VERSION_ERROR])

NAU_ZLIB_VALUEOF([Z_NO_COMPRESSION])
NAU_ZLIB_VALUEOF([Z_BEST_SPEED])
NAU_ZLIB_VALUEOF([Z_BEST_COMPRESSION])
NAU_ZLIB_VALUEOF([Z_DEFAULT_COMPRESSION])

NAU_ZLIB_VALUEOF([Z_FILTERED])
NAU_ZLIB_VALUEOF([Z_HUFFMAN_ONLY])
NAU_ZLIB_VALUEOF([Z_RLE])
NAU_ZLIB_VALUEOF([Z_FIXED])
NAU_ZLIB_VALUEOF([Z_DEFAULT_STRATEGY])

NAU_ZLIB_VALUEOF([Z_BINARY])
NAU_ZLIB_VALUEOF([Z_TEXT])
NAU_ZLIB_VALUEOF([Z_ASCII])
NAU_ZLIB_VALUEOF([Z_UNKNOWN])

NAU_ZLIB_VALUEOF([Z_DEFLATED])

NAU_ZLIB_VALUEOF([Z_NULL])


dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../libraries/zlib/sizeof.sls:libraries/zlib/sizeof.sls.in])
AC_CONFIG_FILES([../libraries/zlib/fields.sls:libraries/zlib/fields.sls.in])
NAUSICAA_END


### end of file
