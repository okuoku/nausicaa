#
# Part of: Nausicaa/Scheme
# Contents: project specific makefile
# Date: Wed Jan 21, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
#
# This program is  free software: you can redistribute  it and/or modify
# it under the  terms of the GNU General Public  License as published by
# the Free Software Foundation, either  version 3 of the License, or (at
# your option) any later version.
#
# This program  is distributed in the  hope that it will  be useful, but
# WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
# MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
# General Public License for more details.
#
# You  should have received  a copy  of the  GNU General  Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#page
## ------------------------------------------------------------
## Configuration variables.
## ------------------------------------------------------------

nau_scheme_ENABLE_BINFMT	= @nau_scheme_ENABLE_BINFMT@


#page
## --------------------------------------------------------------------
## Installation of source and fasl libraries.
## --------------------------------------------------------------------

$(eval $(call nau-libraries,core))

# Lexicographic order please.
$(eval $(call nau-libraries,char-sets,char-sets))
$(eval $(call nau-libraries,csv,csv))
$(eval $(call nau-libraries,email,email))
$(eval $(call nau-libraries,email_addresses,email/addresses))
$(eval $(call nau-sls-libraries,foreign,foreign))
$(eval $(call nau-sls-libraries,foreign_ffi,foreign/ffi))
$(eval $(call nau-sls-libraries,foreign_ffi_pointers,foreign/ffi/pointers))
$(eval $(call nau-sls-libraries,foreign_ffi_peekers-and-pokers,foreign/ffi/peekers-and-pokers))
$(eval $(call nau-sls-libraries,foreign_memory,foreign/memory))
$(eval $(call nau-sls-libraries,foreign_memory_alloc,foreign/memory/alloc))
$(eval $(call nau-sls-libraries,foreign_memory_membuffers,foreign/memory/membuffers))
$(eval $(call nau-sls-libraries,foreign_memory_mempool,foreign/memory/mempool))
$(eval $(call nau-sls-libraries,foreign_memory_operations,foreign/memory/operations))
$(eval $(call nau-libraries,infix,infix))
$(eval $(call nau-libraries,lalr,lalr))
$(eval $(call nau-libraries,lists,lists))
$(eval $(call nau-libraries,nausicaa,nausicaa))
$(eval $(call nau-libraries,parser-tools,parser-tools))
$(eval $(call nau-libraries,profiling,profiling))
$(eval $(call nau-sls-libraries,queues,queues))
$(eval $(call nau-libraries,random,random))
$(eval $(call nau-libraries,records,records))
$(eval $(call nau-libraries,scmobj,scmobj))
$(eval $(call nau-libraries,sexps,sexps))
$(eval $(call nau-libraries,silex,silex))
$(eval $(call nau-sls-libraries,stacks,stacks))
$(eval $(call nau-libraries,strings,strings))
$(eval $(call nau-libraries,times-and-dates,times-and-dates))
$(eval $(call nau-libraries,vectors,vectors))

## --------------------------------------------------------------------

libdist_TMPDIR	= $(TMPDIR)/$(PKG_ID)
libdist_DESTDIR	= $(builddir)/libdist.d
libdist_ARCHIVE	= $(ds_archive_NAME)-$(ds_archive_VERSION)-lib.tar.$(ds_COMPRESSOR_EXT)
libdist_ARCHIVE_PATHNAME= $(libdist_DESTDIR)/$(libdist_ARCHIVE)

.PHONY: libdist

libdist:
	test -d $(libdist_DESTDIR) || $(MKDIR) $(libdist_DESTDIR)
	$(RM_SILENT) $(libdist_TMPDIR)
	$(MAKE) bin-install DESTDIR=$(libdist_TMPDIR)
	yes | $(FIND) $(libdist_TMPDIR)/$(pkglibdir) \
		-type f -and -not -name \*.sls -and -exec rm \{\} \;
	$(TAR) --directory=$(libdist_TMPDIR)/$(pkglibdir) \
		--create $(ds_COMPRESSOR_TAR) --verbose \
		--file=$(libdist_ARCHIVE_PATHNAME) .
	$(RM_SILENT) $(libdist_TMPDIR)

#page
## ------------------------------------------------------------
## Binfmt scripts.
## ------------------------------------------------------------

ifeq ($(strip $(nau_scheme_ENABLE_BINFMT)),yes)

$(eval $(call ds-srcdir,binfmt_scripts,$(srcdir)/src/bin))
$(eval $(call ds-builddir,binfmt_scripts,$(builddir)/bin.d))

binfmt_scripts_SOURCES	= $(addprefix $(binfmt_scripts_SRCDIR)/,		\
	$(call ds-if-yes,$(nausicaa_ENABLE_IKARUS),	ikarus-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_YPSILON),	ypsilon-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_LARCENY),	larceny-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_MOSH),	mosh-scheme-script))
binfmt_scripts_TARGETS	= $(call ds-replace-dir,$(binfmt_scripts_BUILDDIR),$(binfmt_scripts_SOURCES))
binfmt_scripts_INSTLST	= $(binfmt_scripts_TARGETS)
binfmt_scripts_INSTDIR	= $(bindir)

$(eval $(call ds-default-clean-variables,binfmt_scripts))
$(eval $(call ds-module,binfmt_scripts,bin,BIN))

$(binfmt_scripts_TARGETS): $(binfmt_scripts_BUILDDIR)/% : $(binfmt_scripts_SRCDIR)/%
	$(SED) \
		-e 's%__IKARUS__%$(IKARUS)%g'		\
		-e 's%__LARCENY__%$(LARCENY)%g'		\
		-e 's%__MOSH__%$(MOSH)%g'		\
		-e 's%__YPSILON__%$(YPSILON)%g'		\
		<$(<) >$(@)

endif

#page
## ------------------------------------------------------------
## Binfmt configuration.
## ------------------------------------------------------------

ifeq ($(strip $(nau_scheme_ENABLE_BINFMT)),yes)

$(eval $(call ds-srcdir,binfmt_config,$(srcdir)/src/rc.d))
$(eval $(call ds-builddir,binfmt_config,$(builddir)/rc.d))

binfmt_config_SOURCES	= $(binfmt_config_SRCDIR)/rc.scheme
binfmt_config_TARGETS	= $(binfmt_config_BUILDDIR)/rc.scheme
binfmt_config_INSTLST	= $(binfmt_config_TARGETS)
binfmt_config_INSTDIR	= $(sysconfdir)/rc.d

$(eval $(call ds-default-clean-variables,binfmt_config))
$(eval $(call ds-module,binfmt_config,bin,BIN))

$(binfmt_config_TARGETS): $(binfmt_config_SOURCES)
	$(SED) \
	-e 's%BINDIR%$(bindir)%g'				\
	-e 's%OPT_ENABLE_IKARUS%$(nausicaa_ENABLE_IKARUS)%g'	\
	-e 's%OPT_ENABLE_LARCENY%$(nausicaa_ENABLE_LARCENY)%g'	\
	-e 's%OPT_ENABLE_MOSH%$(nausicaa_ENABLE_MOSH)%g'	\
	-e 's%OPT_ENABLE_YPSILON%$(nausicaa_ENABLE_YPSILON)%g'	\
	<$(binfmt_config_SRCDIR)/rc.scheme			\
	>$(binfmt_config_BUILDDIR)/rc.scheme

endif

#page
## ------------------------------------------------------------
## R6RS compatibility tests.
## ------------------------------------------------------------

test_compat_SCRIPT	= $(srcdir)/tests/r6rs/run.sps

.PHONY: test-compat

## ------------------------------------------------------------

ifeq ($(strip $(nausicaa_ENABLE_IKARUS)),yes)
.PHONY: test-ikarus-compat

test-ikarus-compat:
	IKARUS_LIBRARY_PATH=$(srcdir):$(IKARUS_LIBRARY_PATH) \
	$(IKARUS) --r6rs-script $(test_compat_SCRIPT)

test-compat: test-ikarus-compat
endif

## ------------------------------------------------------------

ifeq ($(strip $(nausicaa_ENABLE_LARCENY)),yes)
.PHONY: test-larceny-compat

test-larceny-compat:
	LARCENY_LIBPATH=$(srcdir):$(LARCENY_LIBPATH) \
	$(LARCENY) -r6rs -program  $(test_compat_SCRIPT)

test-compat: test-larceny-compat
endif

## ------------------------------------------------------------

ifeq ($(strip $(nausicaa_ENABLE_MOSH)),yes)
.PHONY: test-mosh-compat

test-mosh-compat:
	MOSH_LOADPATH=$(srcdir):$(MOSH_LOADPATH) \
	$(MOSH) $(test_compat_SCRIPT)

test-compat: test-mosh-compat
endif

## ------------------------------------------------------------

ifeq ($(strip $(nausicaa_ENABLE_YPSILON)),yes)
.PHONY: test-ypsilon-compat

test-ypsilon-compat:
	YPSILON_SITELIB=$(srcdir):$(YPSILON_SITELIB) \
	$(YPSILON) $(test_compat_SCRIPT)

test-compat: test-ypsilon-compat
endif

#page
## --------------------------------------------------------------------
## Special rules: SILex lexers building.
## --------------------------------------------------------------------

SILEX_TEST_PROGRAMS	= $(wildcard $(srcdir)/tests/make-silex-*.sps)
SILEX_LIBPATH		= $(abspath $(srcdir)/tests):$(abspath $(fasl_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_YPSILON))
SILEX_ENV	= YPSILON_SITELIB=$(SILEX_LIBPATH):$(YPSILON_SITELIB)
SILEX_RUNNER	= $(SILEX_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
SILEX_ENV	= IKARUS_LIBRARY_PATH=$(SILEX_LIBPATH):$(IKARUS_LIBRARY_PATH)
SILEX_RUNNER	= $(SILEX_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
SILEX_ENV	= MOSH_LOADPATH=$(SILEX_LIBPATH):$(MOSH_LOADPATH)
SILEX_RUNNER	= $(SILEX_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
SILEX_ENV	= LARCENY_LIBPATH=$(SILEX_LIBPATH):$(LARCENY_LIBPATH)
SILEX_RUNNER	= $(SILEX_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: silex silex-test silex-custom

silex: silex-test

silex-test:
	$(foreach f, $(SILEX_TEST_PROGRAMS),\
		(cd $(srcdir)/tests && $(SILEX_RUNNER) $(f));)

#page
## --------------------------------------------------------------------
## Special rules: LALR test parsers building.
## --------------------------------------------------------------------

LALR_PROGRAMS	= $(wildcard $(srcdir)/tests/make-lalr-*.sps)
LALR_LIBPATH	= $(abspath $(srcdir)/tests):$(abspath $(fasl_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_YPSILON))
LALR_ENV	= YPSILON_SITELIB=$(LALR_LIBPATH):$(YPSILON_SITELIB)
LALR_RUNNER	= $(LALR_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
LALR_ENV	= IKARUS_LIBRARY_PATH=$(LALR_LIBPATH):$(IKARUS_LIBRARY_PATH)
LALR_RUNNER	= $(LALR_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
LALR_ENV	= MOSH_LOADPATH=$(LALR_LIBPATH):$(MOSH_LOADPATH)
LALR_RUNNER	= $(LALR_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
LALR_ENV	= LARCENY_LIBPATH=$(LALR_LIBPATH):$(LARCENY_LIBPATH)
LALR_RUNNER	= $(LALR_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: lalr

lalr:
	$(foreach f, $(LALR_PROGRAMS),(cd $(srcdir)/tests && $(LALR_RUNNER) $(f));)

#page
## --------------------------------------------------------------------
## Special rules: CSV lexers building.
## --------------------------------------------------------------------

CSV_PROGRAM	= make-tables.sps
CSV_LIBPATH	= $(abspath $(fasl_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_YPSILON))
CSV_ENV		= YPSILON_SITELIB=$(CSV_LIBPATH):$(YPSILON_SITELIB)
CSV_RUNNER	= $(CSV_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
CSV_ENV		= IKARUS_LIBRARY_PATH=$(CSV_LIBPATH):$(IKARUS_LIBRARY_PATH)
CSV_RUNNER	= $(CSV_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
CSV_ENV		= MOSH_LOADPATH=$(CSV_LIBPATH):$(MOSH_LOADPATH)
CSV_RUNNER	= $(CSV_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
CSV_ENV		= LARCENY_LIBPATH=$(CSV_LIBPATH):$(LARCENY_LIBPATH)
CSV_RUNNER	= $(CSV_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: csv

csv:
	cd $(srcdir)/src/libraries/csv && $(CSV_RUNNER) $(CSV_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: Infix lexer and parser building.
## --------------------------------------------------------------------

INFIX_LIBPATH	= $(abspath $(fasl_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_YPSILON))
INFIX_ENV	= YPSILON_SITELIB=$(INFIX_LIBPATH):$(YPSILON_SITELIB)
INFIX_RUNNER	= $(INFIX_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
INFIX_ENV	= IKARUS_LIBRARY_PATH=$(INFIX_LIBPATH):$(IKARUS_LIBRARY_PATH)
INFIX_RUNNER	= $(INFIX_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
INFIX_ENV	= MOSH_LOADPATH=$(INFIX_LIBPATH):$(MOSH_LOADPATH)
INFIX_RUNNER	= $(INFIX_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
INFIX_ENV	= LARCENY_LIBPATH=$(INFIX_LIBPATH):$(LARCENY_LIBPATH)
INFIX_RUNNER	= $(INFIX_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: infix

infix:
	cd $(srcdir)/src/libraries/infix && $(INFIX_RUNNER) make-tables.sps

#page
## --------------------------------------------------------------------
## Special rules: email address lexers building.
## --------------------------------------------------------------------

EMAIL_PROGRAM	= make-tables.sps
EMAIL_LIBPATH	= $(abspath $(fasl_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_YPSILON))
EMAIL_ENV		= YPSILON_SITELIB=$(EMAIL_LIBPATH):$(YPSILON_SITELIB)
EMAIL_RUNNER	= $(EMAIL_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
EMAIL_ENV		= IKARUS_LIBRARY_PATH=$(EMAIL_LIBPATH):$(IKARUS_LIBRARY_PATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
EMAIL_ENV		= MOSH_LOADPATH=$(EMAIL_LIBPATH):$(MOSH_LOADPATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
EMAIL_ENV		= LARCENY_LIBPATH=$(EMAIL_LIBPATH):$(LARCENY_LIBPATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: email

email:
	cd $(srcdir)/src/libraries/email/addresses && $(EMAIL_RUNNER) $(EMAIL_PROGRAM)



### end of file
# Local Variables:
# mode: makefile-gmake
# End:
