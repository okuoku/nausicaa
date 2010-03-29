dnl
dnl Part of: Nausicaa/Libxml2
dnl Contents: configuration libxml2
dnl Date: Thu Dec 10, 2009
dnl
dnl Abstract
dnl
dnl
dnl
dnl Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

AC_INIT([nausicaa-libxml2],[0.1b0],[marco.maggi-ipsu@poste.it])
DS_BEGIN([./src/libraries/xml/libxml2.sls])
AC_CANONICAL_TARGET
NAUSICAA_BEGIN
NAUSICAA_SYSTEM_SETUP
DS_EXCLUDE_SECTION([CONFIG_INSPECTION_SCRIPT])
DS_EXCLUDE_SECTION([AUTOCONF])
DS_EXCLUDE_SECTION([PKGCONFIG])
NAUSICAA_SCHEME_CHECK_LIBRARY([NAU_FOREIGN],[(foreign ffi)])
NAUSICAA_C_LANGUAGE

AM_PATH_XML2([2.7.6],,[AC_MSG_FAILURE([cannot find Libxml2 library],[2])])

NAUSICAA_SIZEOF

dnl page
dnl --------------------------------------------------------------------
dnl Helper functions.
dnl --------------------------------------------------------------------

CPPFLAGS="$XML_CPPFLAGS $CPPFLAGS"

m4_define([NAUSICAA_CUSTOM_INCLUDES],[
#ifndef offsetof
#  define offsetof(type, member) ((char *) &((type *) 0)->member - (char *) 0)
#endif

#include <libxml2/libxml/debugXML.h>
#include <libxml2/libxml/nanohttp.h>
#include <libxml2/libxml/tree.h>
#include <libxml2/libxml/xmlexports.h>
#include <libxml2/libxml/xmlstring.h>
#include <libxml2/libxml/HTMLparser.h>
#include <libxml2/libxml/dict.h>
#include <libxml2/libxml/parser.h>
#include <libxml2/libxml/uri.h>
#include <libxml2/libxml/xmlmemory.h>
#include <libxml2/libxml/xmlunicode.h>
#include <libxml2/libxml/HTMLtree.h>
#include <libxml2/libxml/encoding.h>
#include <libxml2/libxml/parserInternals.h>
#include <libxml2/libxml/valid.h>
#include <libxml2/libxml/xmlmodule.h>
#include <libxml2/libxml/xmlversion.h>
#include <libxml2/libxml/SAX.h>
#include <libxml2/libxml/entities.h>
#include <libxml2/libxml/pattern.h>
#include <libxml2/libxml/xinclude.h>
#include <libxml2/libxml/xmlreader.h>
#include <libxml2/libxml/xmlwriter.h>
#include <libxml2/libxml/SAX2.h>
#include <libxml2/libxml/globals.h>
#include <libxml2/libxml/relaxng.h>
#include <libxml2/libxml/xlink.h>
#include <libxml2/libxml/xmlregexp.h>
#include <libxml2/libxml/xpath.h>
#include <libxml2/libxml/c14n.h>
#include <libxml2/libxml/hash.h>
#include <libxml2/libxml/schemasInternals.h>
#include <libxml2/libxml/xmlIO.h>
#include <libxml2/libxml/xmlsave.h>
#include <libxml2/libxml/xpathInternals.h>
#include <libxml2/libxml/catalog.h>
#include <libxml2/libxml/list.h>
#include <libxml2/libxml/schematron.h>
#include <libxml2/libxml/xmlautomata.h>
#include <libxml2/libxml/xmlschemas.h>
#include <libxml2/libxml/xpointer.h>
#include <libxml2/libxml/chvalid.h>
#include <libxml2/libxml/nanoftp.h>
#include <libxml2/libxml/threads.h>
#include <libxml2/libxml/xmlerror.h>
#include <libxml2/libxml/xmlschemastypes.h>
])

NAUSICAA_LIBXML

dnl page
dnl --------------------------------------------------------------------
dnl Done.
dnl --------------------------------------------------------------------

AC_CONFIG_FILES([../src/libraries/xml/libxml2/sizeof.sls:src/libraries/xml/libxml2/sizeof.sls.in])
DS_CONFIG_FILE_SLACKWARE_BIN_DESC
DS_CONFIG_FILE_SLACKWARE_DOC_DESC
NAUSICAA_END
DS_END

### end of file
