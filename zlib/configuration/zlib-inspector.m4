dnl (compression zlib sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Fri Apr 23, 2010
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
dnl
dnl This program is free software:  you can redistribute it and/or modify
dnl it under the terms of the  GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl This program is  distributed in the hope that it  will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
dnl MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl
dnl You should  have received  a copy of  the GNU General  Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.
dnl

AC_DEFUN([NAUSICAA_ZLIB],[

NAUSICAA_INSPECT_TYPE([UINT],[uInt],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([ULONG],[uLong],[unsigned-int],[#f])
NAUSICAA_INSPECT_TYPE([Z_OFF_T],[z_off_t],[unsigned-int],[#f])

dnl Struct inspection: z_stream
NAUSICAA_INSPECT_STRUCT_TYPE([Z_STREAM],[z_stream],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_NEXT_IN],[z_stream],[next_in],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_AVAIL_IN],[z_stream],[avail_in],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_TOTAL_IN],[z_stream],[total_in],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_NEXT_OUT],[z_stream],[next_out],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_AVAIL_OUT],[z_stream],[avail_out],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_TOTAL_OUT],[z_stream],[total_out],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_MSG],[z_stream],[msg],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_DATA_TYPE],[z_stream],[data_type],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_ADLER],[z_stream],[adler],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_ZALLOC],[z_stream],[zalloc],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_ZFREE],[z_stream],[zfree],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([Z_STREAM_OPAQUE],[z_stream],[opaque],[pointer])

dnl Struct inspection: gz_header
NAUSICAA_INSPECT_STRUCT_TYPE([GZ_HEADER],[gz_header],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_TEXT],[gz_header],[text],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_TIME],[gz_header],[time],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_XFLAGS],[gz_header],[xflags],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_OS],[gz_header],[os],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_EXTRA],[gz_header],[extra],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_EXTRA_LEN],[gz_header],[extra_len],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_EXTRA_MAX],[gz_header],[extra_max],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_NAME],[gz_header],[name],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_NAME_MAX],[gz_header],[name_max],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_COMMENT],[gz_header],[comment],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_COMM_MAX],[gz_header],[comm_max],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_HCRC],[gz_header],[hcrc],[signed-int])
NAUSICAA_INSPECT_FIELD_TYPE([GZ_HEADER_DONE],[gz_header],[done],[signed-int])

dnl Preprocessor symbols: version number
NAUSICAA_DEFINE_VALUE([ZLIB_VERNUM])

dnl String preprocessor symbols: version number
NAUSICAA_STRING_TEST([ZLIB_VERSION],[ZLIB_VERSION])

dnl Preprocessor symbols: stream control
NAUSICAA_DEFINE_VALUE([Z_NO_FLUSH])
NAUSICAA_DEFINE_VALUE([Z_PARTIAL_FLUSH])
NAUSICAA_DEFINE_VALUE([Z_SYNC_FLUSH])
NAUSICAA_DEFINE_VALUE([Z_FULL_FLUSH])
NAUSICAA_DEFINE_VALUE([Z_FINISH])
NAUSICAA_DEFINE_VALUE([Z_BLOCK])
NAUSICAA_DEFINE_VALUE([Z_TREES])

dnl Preprocessor symbols: return values
NAUSICAA_DEFINE_VALUE([Z_OK])
NAUSICAA_DEFINE_VALUE([Z_STREAM_END])
NAUSICAA_DEFINE_VALUE([Z_NEED_DICT])
NAUSICAA_DEFINE_VALUE([Z_ERRNO])
NAUSICAA_DEFINE_VALUE([Z_STREAM_ERROR])
NAUSICAA_DEFINE_VALUE([Z_DATA_ERROR])
NAUSICAA_DEFINE_VALUE([Z_MEM_ERROR])
NAUSICAA_DEFINE_VALUE([Z_BUF_ERROR])
NAUSICAA_DEFINE_VALUE([Z_VERSION_ERROR])

dnl Preprocessor symbols: compression level
NAUSICAA_DEFINE_VALUE([Z_NO_COMPRESSION])
NAUSICAA_DEFINE_VALUE([Z_BEST_SPEED])
NAUSICAA_DEFINE_VALUE([Z_BEST_COMPRESSION])
NAUSICAA_DEFINE_VALUE([Z_DEFAULT_COMPRESSION])

dnl Preprocessor symbols: compression algorithm
NAUSICAA_DEFINE_VALUE([Z_FILTERED])
NAUSICAA_DEFINE_VALUE([Z_HUFFMAN_ONLY])
NAUSICAA_DEFINE_VALUE([Z_RLE])
NAUSICAA_DEFINE_VALUE([Z_FIXED])
NAUSICAA_DEFINE_VALUE([Z_DEFAULT_STRATEGY])

dnl Preprocessor symbols: data category
NAUSICAA_DEFINE_VALUE([Z_BINARY])
NAUSICAA_DEFINE_VALUE([Z_TEXT])
NAUSICAA_DEFINE_VALUE([Z_ASCII])
NAUSICAA_DEFINE_VALUE([Z_UNKNOWN])

dnl Preprocessor symbols: miscellaneous
NAUSICAA_DEFINE_VALUE([Z_DEFLATED])
NAUSICAA_DEFINE_VALUE([Z_NULL])
NAU_DS_WITH_OPTION([ZLIB_SHARED_OBJECT],[zlib-shared-object],[libz.so],
  [Zlib shared library file],[select Zlib shared library file])


])


dnl end of file
