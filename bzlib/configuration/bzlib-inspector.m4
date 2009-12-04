dnl (foreign compression bzlib sizeof) --
dnl
dnl Part of: Nausicaa
dnl Contents: foreign library inspection generation
dnl Date: Fri Dec  4, 2009
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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



dnl Preprocessor symbols: control codes
NAUSICAA_DEFINE_VALUE([BZ_RUN])
NAUSICAA_DEFINE_VALUE([BZ_FLUSH])
NAUSICAA_DEFINE_VALUE([BZ_FINISH])

dnl Preprocessor symbols: return values
NAUSICAA_DEFINE_VALUE([BZ_OK])
NAUSICAA_DEFINE_VALUE([BZ_RUN_OK])
NAUSICAA_DEFINE_VALUE([BZ_FLUSH_OK])
NAUSICAA_DEFINE_VALUE([BZ_FINISH_OK])
NAUSICAA_DEFINE_VALUE([BZ_STREAM_END])
NAUSICAA_DEFINE_VALUE([BZ_SEQUENCE_ERROR])
NAUSICAA_DEFINE_VALUE([BZ_PARAM_ERROR])
NAUSICAA_DEFINE_VALUE([BZ_MEM_ERROR])
NAUSICAA_DEFINE_VALUE([BZ_DATA_ERROR])
NAUSICAA_DEFINE_VALUE([BZ_DATA_ERROR_MAGIC])
NAUSICAA_DEFINE_VALUE([BZ_IO_ERROR])
NAUSICAA_DEFINE_VALUE([BZ_UNEXPECTED_EOF])
NAUSICAA_DEFINE_VALUE([BZ_OUTBUFF_FULL])
NAUSICAA_DEFINE_VALUE([BZ_CONFIG_ERROR])

dnl Preprocessor symbols: miscellaneous
NAUSICAA_DEFINE_VALUE([BZ_MAX_UNUSED])

dnl Struct inspection: bz_stream
NAUSICAA_INSPECT_STRUCT_TYPE([BZ_STREAM],[bz_stream],[#f])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_NEXT_IN],[bz_stream],[next_in],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_AVAIL_IN],[bz_stream],[avail_in],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_TOTAL_IN_LO32],[bz_stream],[total_in_lo32],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_TOTAL_IN_HI32],[bz_stream],[total_in_hi32],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_NEXT_OUT],[bz_stream],[next_out],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_AVAIL_OUT],[bz_stream],[avail_out],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_TOTAL_OUT_LO32],[bz_stream],[total_out_lo32],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_TOTAL_OUT_HI32],[bz_stream],[total_out_hi32],[unsigned-int])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_STATE],[bz_stream],[state],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_BZALLOC],[bz_stream],[bzalloc],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_BZFREE],[bz_stream],[bzfree],[pointer])
NAUSICAA_INSPECT_FIELD_TYPE([BZ_STREAM_OPAQUE],[bz_stream],[opaque],[pointer])
NAU_DS_WITH_OPTION([BZLIB_SHARED_OBJECT],[bzlib-shared-object],[libbz2.so],
  [Bzlib shared library file],[select Bzlib shared library file])

dnl end of file
