dnl
dnl Part of: Nausicaa/Zlib
dnl Contents: configuration template
dnl Date: Sun Dec  7, 2008
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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

AC_INIT([nausicaa-zlib],[0.1b0],[marcomaggi@gna.org])
DS_BEGIN([./src/libraries/foreign/zlib.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
AC_CHECK_HEADERS([zlib.h],,[AC_MSG_FAILURE([cannot find libz header],[2])])
AC_CHECK_LIB([z],[deflate],,[AC_MSG_FAILURE([cannot find libz library],[2])])
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Zlib header inspection.
dnl --------------------------------------------------------------------

NAUSICAA_SIZEOF_TEST([ZSTREAM],[z_stream],,[#include <zlib.h>])

AC_DEFUN([NAU_ZLIB_STREAM_FIELD],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[z_stream],[$2],[$3],,[#include <zlib.h>])])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_NEXT_IN],[next_in],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_AVAIL_IN],[avail_in],[unsigned-int])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_TOTAL_IN],[total_in],[unsigned-int])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_NEXT_OUT],[next_out],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_AVAIL_OUT],[avail_out],[unsigned-int])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_TOTAL_OUT],[total_out],[unsigned-int])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_MSG],[msg],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_DATA_TYPE],[data_type],[signed-int])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_ADLER],[adler],[signed-int])

NAU_ZLIB_STREAM_FIELD([ZSTREAM_ZALLOC],[zalloc],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_ZFREE],[zfree],[pointer])
NAU_ZLIB_STREAM_FIELD([ZSTREAM_OPAQUE],[opaque],[pointer])

dnl --------------------------------------------------------------------

NAUSICAA_SIZEOF_TEST([GZHEADER],[gz_header],,[#include <zlib.h>])

AC_DEFUN([NAU_ZLIB_HEADER_FIELD],
  [NAUSICAA_INSPECT_FIELD_TYPE([$1],[gz_header],[$2],[$3],,[#include <zlib.h>])])

NAU_ZLIB_HEADER_FIELD([GZHEADER_TEXT],[text],[signed-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_TIME],[time],[unsigned-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_XFLAGS],[xflags],[signed-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_OS],[os],[signed-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_EXTRA],[extra],[pointer])
NAU_ZLIB_HEADER_FIELD([GZHEADER_EXTRA_LEN],[extra_len],[unsigned-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_EXTRA_MAX],[extra_max],[unsigned-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_NAME],[name],[pointer])
NAU_ZLIB_HEADER_FIELD([GZHEADER_NAME_MAX],[name_max],[unsigned-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_COMMENT],[comment],[pointer])
NAU_ZLIB_HEADER_FIELD([GZHEADER_COMM_MAX],[comm_max],[unsigned-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_HCRC],[hcrc],[signed-int])
NAU_ZLIB_HEADER_FIELD([GZHEADER_DONE],[done],[signed-int])

dnl --------------------------------------------------------------------

NAUSICAA_INSPECT_TYPE([ZLIB_UINT],[uInt],[unsigned-int],,[#include <zlib.h>])
NAUSICAA_INSPECT_TYPE([ZLIB_ULONG],[uLong],[unsigned-int],,[#include <zlib.h>])
NAUSICAA_INSPECT_TYPE([Z_OFF_T],[z_off_t],[unsigned-int],,[#include <zlib.h>])

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

AC_CONFIG_FILES([../src/libraries/foreign/zlib/sizeof.sls:src/libraries/foreign/zlib/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
