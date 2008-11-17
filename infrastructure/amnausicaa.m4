divert(-1)dnl
dnl 
dnl Part of: Nausicaa
dnl Contents: makefile blocks
dnl Date: Mon Nov 17, 2008
dnl 
dnl Abstract
dnl 
dnl 
dnl 
dnl Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
dnl 
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free  Software Foundation, either  version 3 of the  License, or
dnl (at your option) any later version.
dnl 
dnl This program is distributed in the  hope that it will be useful, but
dnl WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
dnl MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See  the GNU
dnl General Public License for more details.
dnl 
dnl You should  have received a copy  of the GNU  General Public License
dnl along      with      this       program.       If      not,      see
dnl <http://www.gnu.org/licenses/>.
dnl 

dnl page
dnl --------------------------------------------------------------------
dnl Main blocks.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl 
dnl   NAUSICAA_BEGIN
dnl 
dnl Arguments:
dnl 
dnl   .
dnl 
dnl Description:
dnl 
dnl   Open a Nausicaa Makefile.
dnl
dnl Usage examples:
dnl
dnl   .
dnl
define([NAUSICAA_BEGIN],[dnl
NAUSICAA_MAIN_VARIABLES
DS_CONFIGURATION_VARIABLES
NAUSICAA_CONFIGURATION_VARIABLES
NAUSICAA_BINARY_PACKAGE_VARIABLES
DS_BINARY_PACKAGE_VARIABLES
DS_DIRECTORIES 
NAUSICAA_DIRECTORIES
DS_PROGRAMS
NAUSICAA_PROGRAMS
DS_MAIN_PHONY_RULES
])

dnl Synopsis:
dnl 
dnl   NAUSICAA_CLOSE
dnl 
dnl Arguments:
dnl 
dnl   .
dnl 
dnl Description:
dnl 
dnl   Close a Nausicaa Makefile.
dnl
dnl Usage examples:
dnl
dnl   .
dnl
define([NAUSICAA_END],[dnl
NAUSICAA_SOURCE_LIBRARIES
NAUSICAA_PRECOMPILED_LIBRARIES
NAUSICAA_TESTS
DS_DOC_TEXINFO
DS_DOC_TEXINFO_INFO
DS_DOC_TEXINFO_HTML
DS_BINARY_DISTRIBUTION
DS_SLACKWARE_PACKAGING
DS_REDHAT_PACKAGING
DS_END
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Main and configuration variables.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([NAUSICAA_MAIN_VARIABLES],
		[Global variables and default values.],[
PACKAGE_NAME		= @PACKAGE_NAME@
PACKAGE_VERSION		= @PACKAGE_VERSION@
PKG_ID			= @PKG_ID@
])

dnl -------------------------------------------------------------------- 

DS_DEFINE_BLOCK([NAUSICAA_CONFIGURATION_VARIABLES],
		[Nausicaa configuration variables.],[
# If set to 'yes' source files will be installed.
nausicaa_ENABLE_SLS	= @nausicaa_ENABLE_SLS@

# If set to 'yes' fasl files will be produced and installed.
nausicaa_ENABLE_FASL	= @nausicaa_ENABLE_FASL@
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Binary package variables customisation.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([NAUSICAA_BINARY_PACKAGE_VARIABLES],
		[Nausicaa pre-configuration of binary package variables.],[
ifeq ($(strip $(package_BUILD_VERSION)),)
package_BUILD_VERSION	= 1nau
endif
])

dnl --------------------------------------------------------------------
 
dnl page
dnl --------------------------------------------------------------------
dnl Directories and programs.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([NAUSICAA_DIRECTORIES],[Nausicaa directories.],[
ikarus_PKGLIBDIR	= $(libdir)/ikarus
])

DS_DEFINE_BLOCK([NAUSICAA_PROGRAMS],[Nausicaa programs.],[
IKARUS		= @IKARUS@
SCHEME_SCRIPT	= @SCHEME_SCRIPT@
])

dnl --------------------------------------------------------------------
 
dnl page
dnl --------------------------------------------------------------------
dnl Source files.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([NAUSICAA_SOURCE_LIBRARIES],[Library source files.],[
# Find the list of directories under "libraries".
sls_FIND_DIRS		= cd $(sls_SRCDIR) ; $(FIND) . -type d

# Find the reversed list of directories under "libraries".
sls_FIND_DIRS_REV	= cd $(sls_SRCDIR); \
		ls -1d $(DIRECTORIES) | $(SORT) --reverse

# Find  all  the source  files.   Only  the  .sls files  are
# considered for compilation and installation.
sls_FIND_FILES		= cd $(sls_SRCDIR) ; $(FIND) . -type f	\
			-and -name '*.sls'		\
			-and -print

DS_COMMENT_SEPARATOR

sls_SRCDIR		= $(srcdir)/libraries

# We sort  the list of  directories so that  when installing
# them  we can create  the parents  first (useless  with the
# "install"  program from GNU  Coreutils, but  we do  it the
# same).
sls_DIRECTORIES			= $(sort $(shell $(sls_FIND_DIRS)))

# The reversed  list of  directories allows us  to uninstall
# the children first: the children directories have a longer
# pathname.
sls_REVERSED_DIRECTORIES	= $(shell $(sls_FIND_DIRS_REV))

sls_SOURCES			= $(shell $(sls_FIND_FILES))
sls_INSTLST			= $(sls_SOURCES)
sls_INSTDIR			= $(ikarus_PKGLIBDIR)

DS_COMMENT_SEPARATOR
 
.PHONY: sls-install sls-uninstall

ifeq ($(strip $(nausicaa_ENABLE_SLS)),yes)

sls-install:
	$(INSTALL_DIR) $(foreach d,$(sls_DIRECTORIES),$(DESTDIR)$(sls_INSTDIR)/$(d))
	cd $(sls_SRCDIR); $(foreach f,$(sls_SOURCES),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(sls_INSTDIR)/$(dir $(f));)

sls-uninstall:
	-@$(RM) $(addprefix $(DESTDIR)$(sls_INSTDIR)/,$(FILES))
	-@$(RMDIR) $(addprefix $(DESTDIR)$(sls_INSTDIR)/,$(REVERSED_DIRECTORIES))

else
sls-install sls-uninstall:
endif

DS_COMMENT_SEPARATOR

install:		sls-install
uninstall:		sls-uninstall
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Precompiled libraries.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([NAUSICAA_PRECOMPILED_LIBRARIES],
		[Library precompiled files.],[
fasl_SRCDIR		= $(sls_SRCDIR)
fasl_BUILDDIR		= $(builddir)/fasl.d

fasl_FASL_EXTENSION	= .ikarus-fasl

DS_COMMENT_SEPARATOR

# Find the FASL files that currently exists in the source
# directory.
fasl_FIND_FASL_SRCDIR	= \
	cd $(fasl_SRCDIR) ; \
	$(FIND) . -type f -and -name "*$(fasl_FASL_EXTENSION)" -and -print

# Find the FASL files that currently exists in the build
# directory.
fasl_FIND_FASL_BUILDDIR	= \
	cd $(fasl_BUILDDIR) ; \
	$(FIND) . -type f -and -name "*$(fasl_FASL_EXTENSION)" -and -print

fasl_COMPILE_SCRIPT	= compile-all.sps
fasl_COMPILE_ENV	= IKARUS_LIBRARY_PATH=$(fasl_SRCDIR)
fasl_COMPILE_COMMAND	= $(IKARUS) --compile-dependencies
fasl_COMPILE_RUN	= $(fasl_COMPILE_ENV) $(fasl_COMPILE_COMMAND) $(fasl_COMPILE_SCRIPT)

DS_COMMENT_SECTION

fasl_FASL_FILES		= $(shell $(fasl_FIND_FASL_SRCDIR))
fasl_TARGETS		= $(shell $(fasl_FIND_FASL_BUILDDIR))

fasl_INSTLST		= $(fasl_TARGETS)
fasl_INSTDIR		= $(sls_INSTDIR)

DS_COMMENT_SEPARATOR

.PHONY: fasl fasl-clean fasl-clean-fasl-in-srcdir
.PHONY: fasl-install fasl-uninstall

ifeq ($(strip $(nausicaa_ENABLE_FASL)),yes)

## Notice  that,  by default,  GNU  tar  preserves the  file
## modification times  so the FASL times will  be newer than
## the corresponding .sls times.
##
## When these files are installed: loading will work fine if
## we give  the appropriate preserve flags  to the "install"
## program.
fasl: $(fasl_BUILDDIR)
	cd $(fasl_SRCDIR) ; test -f $(fasl_COMPILE_SCRIPT) && $(fasl_COMPILE_RUN)
	$(fasl_FIND_FASL_SRCDIR) | \
	$(TAR) --create  --file=- --files-from=- | \
	$(TAR) --directory=$(PWD)/$(fasl_BUILDDIR) --extract --verbose --file=-
	$(MAKE) fasl-clean-fasl-in-srcdir

$(fasl_BUILDDIR):
	-@test -d $(fasl_BUILDDIR) || $(MKDIR) $(fasl_BUILDDIR)

fasl-clean: fasl-clean-fasl-in-srcdir
	$(RM) $(fasl_BUILDDIR)/*

fasl-clean-fasl-in-srcdir:
	-cd $(fasl_SRCDIR); $(RM) $(fasl_FASL_FILES)

fasl-install:
	$(INSTALL_DIR) $(foreach d,$(sls_DIRECTORIES),$(DESTDIR)$(fasl_INSTDIR)/$(d))
	cd $(fasl_BUILDDIR); $(foreach f,$(fasl_TARGETS),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(fasl_INSTDIR)/$(dir $(f));)

fasl-uninstall:
	cd $(fasl_BUILDDIR); $(foreach f,$(fasl_TARGETS),\
		$(INSTALL_DATA) $(f) $(DESTDIR)$(fasl_INSTDIR)/$(dir $(f));)
	-@$(RMDIR) $(addprefix $(DESTDIR)$(fasl_INSTDIR)/,$(REVERSED_DIRECTORIES))

else
fasl fasl-clean fasl-clean-fasl-in-srcdir:
fasl-install fasl-uninstall:
endif

DS_COMMENT_SEPARATOR

.PHONY: compile compile-clean

compile:	fasl
compile-clean:	fasl-clean

all:		fasl
clean:		fasl-clean

install:	fasl-install
uninstall:	fasl-uninstall
])

dnl --------------------------------------------------------------------
 
dnl page
dnl --------------------------------------------------------------------
dnl Tests.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([NAUSICAA_TESTS],[Testing.],[
nau_test_SRCDIR		= $(srcdir)/tests
nau_test_FILES		= $(wildcard $(nau_test_SRCDIR)/test-*.sps)

# We DO  NOT include the  fasl build directory in  the path.
# This is on purpose.
nau_test_ENV		= IKARUS_LIBRARY_PATH=$(srcdir)/libraries:$(srcdir)/tests
nau_test_RUN		= $(nau_test_ENV) $(SCHEME_SCRIPT)

nau_test_SELECTED_FILES	= $(wildcard $(nau_test_SRCDIR)/test-$(FILE).sps)

.PHONY: tests test check test-file

tests test check:
	$(foreach f,$(nau_test_FILES),$(nau_test_RUN) $(f);)

test-file:
	$(foreach f,$(nau_test_SELECTED_FILES),$(nau_test_RUN) $(f);)

])


dnl end of file
divert(0)dnl
