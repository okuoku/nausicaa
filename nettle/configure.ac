dnl
dnl Part of: Nausicaa/Nettle
dnl Contents: configuration nettle
dnl Date:
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-nettle],[0.1d0],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/foreign/crypto/nettle.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE
AC_CHECK_LIB([gmp],[__gmpz_init],,[AC_MSG_FAILURE([missing GMP library],[2])])
AC_CHECK_HEADERS([gmp.h],,[AC_MSG_FAILURE([missing gmp.h header],[2])])
AC_CHECK_HEADERS([nettle/aes.h],,[AC_MSG_FAILURE([cannot find Nettle header],[2])])
AC_CHECK_LIB([nettle],[nettle_aes_encrypt],,[AC_MSG_FAILURE([cannot find Nettle library],[2])])
AC_CHECK_LIB([hogweed],[nettle_rsa_public_key_init],,[AC_MSG_FAILURE([cannot find Nettle library],[2])])
NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#include <nettle/aes.h>
#include <nettle/arcfour.h>
#include <nettle/arctwo.h>
#include <nettle/asn1.h>
#include <nettle/base16.h>
#include <nettle/base64.h>
#include <nettle/bignum.h>
#include <nettle/blowfish.h>
#include <nettle/buffer.h>
#include <nettle/cast128.h>
#include <nettle/cbc.h>
#include <nettle/ctr.h>
#include <nettle/des.h>
#include <nettle/dsa.h>
#include <nettle/hmac.h>
#include <nettle/knuth-lfib.h>
#include <nettle/md2.h>
#include <nettle/md4.h>
#include <nettle/md5.h>
#include <nettle/memxor.h>
#include <nettle/pgp.h>
#include <nettle/pkcs1.h>
#include <nettle/rsa.h>
#include <nettle/serpent.h>
#include <nettle/sexp.h>
#include <nettle/sha.h>
#include <nettle/twofish.h>
#include <nettle/yarrow.h>
])

m4_include([configuration/nettle-inspector.m4])

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/foreign/crypto/nettle/sizeof.sls:src/libraries/foreign/crypto/nettle/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
