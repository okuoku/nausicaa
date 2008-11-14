# 
# Part of: Nausicaa
# Contents: maintainer rules
# Date: Thu Nov 13, 2008
# 
# Abstract
# 
#	This  file  defines rules  for  maintaining the  Nausicaa
#	distribution.   It is NOT  meant to  be used  by ordinary
#	users of the packages.
# 
# Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
# 
# This program  is free software: you can  redistribute it and/or
# modify it under the terms  of the GNU General Public License as
# published by the Free  Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of
# MERCHANTABILITY or  FITNESS FOR A PARTICULAR  PURPOSE.  See the
# GNU General Public License for more details.
# 
# You  should have  received a  copy  of the  GNU General  Public
# License    along   with    this   program.     If    not,   see
# <http://www.gnu.org/licenses/>.
# 

#page
## ------------------------------------------------------------
## Global variables.
## ------------------------------------------------------------

TAG			= $(shell cat tag)
PKG_ID			= $(TAG)

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Programs.
## ------------------------------------------------------------

CP		= cp --force --verbose --preserve=mode --
GIT		= git
MKDIR		= mkdir --parents --verbose
MV		= mv --verbose --
RM		= rm --force --recursive --verbose --
RM_SILENT	= rm --force --recursive --
RMDIR		= rmdir --parents --ignore-fail-on-non-empty --
TAR		= tar


## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Directories.
## ------------------------------------------------------------

ifeq ($(strip $(TMPDIR)),)
TMPDIR		= /tmp
endif

srcdir		= .
builddir	= "=build"


## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Main rules.
## ------------------------------------------------------------

.PHONY: all tag builddir

all:

tag:
	echo $(lastword $(shell $(GIT) tag)) >tag
	printf '@macro version{}\n%s\n@end macro' $(TAG) \
		>doc/version.texiinc

builddir:
	-@test -d $(builddir) || $(MKDIR) $(builddir)

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Global tests.
## ------------------------------------------------------------

test_BUILDDIR	= "=builddir"
test_PROJECTS	= \
			srfi		\
			ice-9		\
			irregex		\
			scmobj		\
			tiny-clos	\
			uriel

.PHONY: test

test:
	$(foreach p,$(test_PROJECTS),\
	cd $(p);						\
	test -d $(test_BUILDDIR) || $(MKDIR) $(test_BUILDDIR);	\
	cd $(test_BUILDDIR);					\
	sh ../prepare.sh;					\
	make all test;						\
	cd ../..;)


## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Source distribution.
## ------------------------------------------------------------

.PHONY: dist

dist_TMPDIR		= $(TMPDIR)/$(PKG_ID)
dist_ARCHIVE		= $(PKG_ID)-src.tar.bz2
dist_DESTDIR		= $(builddir)/dist.d

dist: builddir tag
	-test -d $(dist_DESTDIR) || $(MKDIR) $(dist_DESTDIR)
	$(RM_SILENT) $(dist_TMPDIR)
	$(RM_SILENT) $(TMPDIR)/$(dist_ARCHIVE)
	$(MKDIR) $(dist_TMPDIR)
	$(TAR) \
		--directory=$(srcdir) --create --file=- --dereference		\
		--exclude=RCS                   --exclude=CVS                   \
		--exclude=.git			--exclude=.git\*		\
		--exclude=archives              --exclude=\*.ps			\
		--exclude=\*.dvi                --exclude=tmp			\
		--exclude=\*.gz                 --exclude=\*.tar                \
		--exclude=\*.so                 --exclude=\*.o                  \
		--exclude=\*.a                  --exclude=\*.rpm                \
		--exclude=\*.deb                --exclude=.emacs\*		\
		--exclude=\*~                   --exclude=TAGS                  \
		--exclude=config.log            --exclude=config.status         \
		--exclude=config.cache          --exclude=Makefile              \
		--exclude=autom4te.cache	--exclude="{arch}"              \
		--exclude=.arch-ids		--exclude=\+\+\*                \
		--exclude=\=\*                  --exclude=\*.ikarus-fasl        \
		--exclude=\*.tgz						\
		. | $(TAR) --directory=$(dist_TMPDIR) --extract --file=-
	$(TAR) --directory=$(TMPDIR) --verbose \
		--create --bzip2 --file=$(dist_DESTDIR)/$(dist_ARCHIVE) $(PKG_ID)
	$(RM_SILENT) $(dist_TMPDIR)


## ------------------------------------------------------------



### end of file
# Local Variables:
# fill-column: 65
# End:
