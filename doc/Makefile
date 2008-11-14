# 
# Part of: Nausicaa
# Contents: documentation rules
# Date: Thu Nov 13, 2008
# 
# Abstract
# 
# 
# 
# Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
# 
# This  program  is free  software:  you  can redistribute  it
# and/or modify it  under the terms of the  GNU General Public
# License as published by the Free Software Foundation, either
# version  3 of  the License,  or (at  your option)  any later
# version.
# 
# This  program is  distributed in  the hope  that it  will be
# useful, but  WITHOUT ANY WARRANTY; without  even the implied
# warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
# PURPOSE.   See  the  GNU  General Public  License  for  more
# details.
# 
# You should  have received a  copy of the GNU  General Public
# License   along   with    this   program.    If   not,   see
# <http://www.gnu.org/licenses/>.
# 

#page
## ------------------------------------------------------------
## Directories.
## ------------------------------------------------------------

srcdir		= .
builddir	= .

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Programs.
## ------------------------------------------------------------

RM			= rm --force --recursive --

MAKEINFO		= makeinfo
MAKEINFO_INFO_FLAGS	= --no-split
MAKEINFO_HTML_FLAGS	= --no-split --html


## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Main rules.
## ------------------------------------------------------------

.PHONY: all doc clean

all: doc

doc:

clean:
	-$(RM) nausicaa.info nausicaa.html

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Texinfo documentation rules.
## ------------------------------------------------------------

doc_texinfo_TARGETS	= nausicaa.info nausicaa.html

nausicaa.info: nausicaa.texi
	$(MAKEINFO) $(MAKEINFO_INFO_FLAGS) -o $(@) $(<)

nausicaa.html: nausicaa.texi
	$(MAKEINFO) $(MAKEINFO_HTML_FLAGS) -o $(@) $(<)

doc: $(doc_texinfo_TARGETS)

## ------------------------------------------------------------



### end of file
