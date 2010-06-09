#
# Part of: Nausicaa/Scheme
# Contents: project specific makefile
# Date: Wed Jan 21, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
$(eval $(call nau-libraries,armor,armor))
$(eval $(call nau-libraries,char-sets,char-sets))
$(eval $(call nau-libraries,cond-expand,cond-expand))
$(eval $(call nau-libraries,classes,classes))
$(eval $(call nau-libraries,csv,csv))
$(eval $(call nau-libraries,email,email))
$(eval $(call nau-libraries,email_addresses,email/addresses))
$(eval $(call nau-libraries,ffi,ffi))
$(eval $(call nau-libraries,ffi_clang_data_types,ffi/clang-data-types))
$(eval $(call nau-libraries,ffi_pointers,ffi/pointers))
$(eval $(call nau-libraries,ffi_peekers-and-pokers,ffi/peekers-and-pokers))
$(eval $(call nau-libraries,ffi_memory,ffi/memory))
$(eval $(call nau-libraries,ffi_memory_alloc,ffi/memory/alloc))
$(eval $(call nau-libraries,infix,infix))
$(eval $(call nau-libraries,json,json))
$(eval $(call nau-libraries,lalr,lalr))
$(eval $(call nau-libraries,libraries,libraries))
$(eval $(call nau-libraries,lists,lists))
$(eval $(call nau-libraries,makers,makers))
##$(eval $(call nau-libraries,msgcat,msgcat))
$(eval $(call nau-libraries,nausicaa,nausicaa))
$(eval $(call nau-libraries,net,net))
$(eval $(call nau-libraries,net_helpers,net/helpers))
$(eval $(call nau-libraries,parser-tools,parser-tools))
$(eval $(call nau-libraries,profiling,profiling))
$(eval $(call nau-libraries,randomisations,randomisations))
$(eval $(call nau-libraries,scmobj,scmobj))
$(eval $(call nau-libraries,silex,silex))
$(eval $(call nau-libraries,strings,strings))
$(eval $(call nau-libraries,times-and-dates,times-and-dates))
$(eval $(call nau-libraries,uri,uri))
$(eval $(call nau-libraries,vectors,vectors))

#page
## --------------------------------------------------------------------
## Message catalogs.
## --------------------------------------------------------------------

$(eval $(call ds-srcdir,msgcat,$(srcdir)/src/libraries/msgcat))
$(eval $(call ds-builddir,msgcat,$(nau_sls_BUILDDIR)/msgcat))

msgcat_SOURCES	= $(call ds-glob,msgcat,*.cat)
msgcat_TARGETS	= $(call ds-replace-dir,$(msgcat_BUILDDIR),$(msgcat_SOURCES))

msgcat_INSTDIR	= $(pkglibdir)/msgcat
msgcat_INSTLST	= $(msgcat_TARGETS)

$(eval $(call ds-module,msgcat,bin,DATA))

$(msgcat_BUILDDIR)/%.cat : $(msgcat_SRCDIR)/%.cat
	$(CP) $(<) $(@)

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
	$(call ds-if-yes,$(nausicaa_ENABLE_MOSH),	mosh-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_PETITE),	petite-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_VICARE),	vicare-scheme-script))
binfmt_scripts_TARGETS	= $(call ds-replace-dir,$(binfmt_scripts_BUILDDIR),$(binfmt_scripts_SOURCES))
binfmt_scripts_INSTLST	= $(binfmt_scripts_TARGETS)
binfmt_scripts_INSTDIR	= $(pkglibexecdir)

$(eval $(call ds-default-clean-variables,binfmt_scripts))
$(eval $(call ds-module,binfmt_scripts,bin,BIN))

$(binfmt_scripts_TARGETS): $(binfmt_scripts_BUILDDIR)/% : $(binfmt_scripts_SRCDIR)/%
	$(SED) \
		-e 's%__IKARUS__%$(IKARUS)%g'		\
		-e 's%__LARCENY__%$(LARCENY)%g'		\
		-e 's%__MOSH__%$(MOSH)%g'		\
		-e 's%__PETITE__%$(PETITE)%g'		\
		-e 's%__YPSILON__%$(YPSILON)%g'		\
		-e 's%__VICARE__%$(VICARE)%g'		\
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
	-e 's%BINDIR%$(binfmt_scripts_INSTDIR)%g'		\
	-e 's%OPT_ENABLE_IKARUS%$(nausicaa_ENABLE_IKARUS)%g'	\
	-e 's%OPT_ENABLE_LARCENY%$(nausicaa_ENABLE_LARCENY)%g'	\
	-e 's%OPT_ENABLE_MOSH%$(nausicaa_ENABLE_MOSH)%g'	\
	-e 's%OPT_ENABLE_PETITE%$(nausicaa_ENABLE_PETITE)%g'	\
	-e 's%OPT_ENABLE_YPSILON%$(nausicaa_ENABLE_YPSILON)%g'	\
	-e 's%OPT_ENABLE_VICARE%$(nausicaa_ENABLE_VICARE)%g'	\
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

ifeq ($(strip $(nausicaa_ENABLE_PETITE)),yes)
.PHONY: test-petite-compat

test-petite-compat:
	PETITE_LIBPATH=$(srcdir):$(PETITE_LIBPATH) \
	$(PETITE) --libdirs $${PETITE_LIBPATH} --program $(test_compat_SCRIPT)

test-compat: test-petite-compat
endif

## ------------------------------------------------------------

ifeq ($(strip $(nausicaa_ENABLE_VICARE)),yes)
.PHONY: test-vicare-compat

test-vicare-compat:
	VICARE_LIBRARY_PATH=$(srcdir):$(VICARE_LIBRARY_PATH) \
	$(VICARE) --r6rs-script $(test_compat_SCRIPT)

test-compat: test-vicare-compat
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
SILEX_LIBPATH		= $(abspath $(srcdir)/tests):$(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
SILEX_ENV	= VICARE_LIBRARY_PATH=$(SILEX_LIBPATH):$(VICARE_LIBRARY_PATH)
SILEX_RUNNER	= $(SILEX_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
SILEX_ENV	= IKARUS_LIBRARY_PATH=$(SILEX_LIBPATH):$(IKARUS_LIBRARY_PATH)
SILEX_RUNNER	= $(SILEX_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
SILEX_ENV	= MOSH_LOADPATH=$(SILEX_LIBPATH):$(MOSH_LOADPATH)
SILEX_RUNNER	= $(SILEX_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
SILEX_ENV	= PETITE_LIBPATH=$(SILEX_LIBPATH):$(PETITE_LIBPATH)
SILEX_RUNNER	= $(SILEX_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
SILEX_ENV	= YPSILON_SITELIB=$(SILEX_LIBPATH):$(YPSILON_SITELIB)
SILEX_RUNNER	= $(SILEX_ENV) $(YPSILON)
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
LALR_LIBPATH	= $(abspath $(srcdir)/tests):$(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
LALR_ENV	= VICARE_LIBRARY_PATH=$(LALR_LIBPATH):$(VICARE_LIBRARY_PATH)
LALR_RUNNER	= $(LALR_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
LALR_ENV	= IKARUS_LIBRARY_PATH=$(LALR_LIBPATH):$(IKARUS_LIBRARY_PATH)
LALR_RUNNER	= $(LALR_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
LALR_ENV	= MOSH_LOADPATH=$(LALR_LIBPATH):$(MOSH_LOADPATH)
LALR_RUNNER	= $(LALR_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
LALR_ENV	= PETITE_LIBPATH=$(LALR_LIBPATH):$(PETITE_LIBPATH)
LALR_RUNNER	= $(LALR_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
LALR_ENV	= YPSILON_SITELIB=$(LALR_LIBPATH):$(YPSILON_SITELIB)
LALR_RUNNER	= $(LALR_ENV) $(YPSILON)
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
CSV_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
CSV_ENV		= VICARE_LIBRARY_PATH=$(CSV_LIBPATH):$(VICARE_LIBRARY_PATH)
CSV_RUNNER	= $(CSV_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
CSV_ENV		= IKARUS_LIBRARY_PATH=$(CSV_LIBPATH):$(IKARUS_LIBRARY_PATH)
CSV_RUNNER	= $(CSV_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
CSV_ENV		= MOSH_LOADPATH=$(CSV_LIBPATH):$(MOSH_LOADPATH)
CSV_RUNNER	= $(CSV_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
CSV_ENV		= PETITE_LIBPATH=$(CSV_LIBPATH):$(PETITE_LIBPATH)
CSV_RUNNER	= $(CSV_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
CSV_ENV		= YPSILON_SITELIB=$(CSV_LIBPATH):$(YPSILON_SITELIB)
CSV_RUNNER	= $(CSV_ENV) $(YPSILON)
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

INFIX_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
INFIX_ENV	= VICARE_LIBRARY_PATH=$(INFIX_LIBPATH):$(VICARE_LIBRARY_PATH)
INFIX_RUNNER	= $(INFIX_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
INFIX_ENV	= IKARUS_LIBRARY_PATH=$(INFIX_LIBPATH):$(IKARUS_LIBRARY_PATH)
INFIX_RUNNER	= $(INFIX_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
INFIX_ENV	= MOSH_LOADPATH=$(INFIX_LIBPATH):$(MOSH_LOADPATH)
INFIX_RUNNER	= $(INFIX_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
INFIX_ENV	= PETITE_LIBPATH=$(INFIX_LIBPATH):$(PETITE_LIBPATH)
INFIX_RUNNER	= $(INFIX_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
INFIX_ENV	= YPSILON_SITELIB=$(INFIX_LIBPATH):$(YPSILON_SITELIB)
INFIX_RUNNER	= $(INFIX_ENV) $(YPSILON)
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
EMAIL_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
EMAIL_ENV	= VICARE_LIBRARY_PATH=$(EMAIL_LIBPATH):$(VICARE_LIBRARY_PATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
EMAIL_ENV		= IKARUS_LIBRARY_PATH=$(EMAIL_LIBPATH):$(IKARUS_LIBRARY_PATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
EMAIL_ENV		= MOSH_LOADPATH=$(EMAIL_LIBPATH):$(MOSH_LOADPATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
EMAIL_ENV		= PETITE_LIBPATH=$(EMAIL_LIBPATH):$(PETITE_LIBPATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
EMAIL_ENV		= YPSILON_SITELIB=$(EMAIL_LIBPATH):$(YPSILON_SITELIB)
EMAIL_RUNNER	= $(EMAIL_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
EMAIL_ENV		= LARCENY_LIBPATH=$(EMAIL_LIBPATH):$(LARCENY_LIBPATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: email

email:
	cd $(srcdir)/src/libraries/email/addresses && $(EMAIL_RUNNER) $(EMAIL_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: json lexers and parser building.
## --------------------------------------------------------------------

JSON_PROGRAM	= make-tables.sps
JSON_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
JSON_ENV	= VICARE_LIBRARY_PATH=$(JSON_LIBPATH):$(VICARE_LIBRARY_PATH)
JSON_RUNNER	= $(JSON_ENV) $(VICARE) --debug --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
JSON_ENV		= IKARUS_LIBRARY_PATH=$(JSON_LIBPATH):$(IKARUS_LIBRARY_PATH)
JSON_RUNNER	= $(JSON_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
JSON_ENV		= MOSH_LOADPATH=$(JSON_LIBPATH):$(MOSH_LOADPATH)
JSON_RUNNER	= $(JSON_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
JSON_ENV		= PETITE_LIBPATH=$(JSON_LIBPATH):$(PETITE_LIBPATH)
JSON_RUNNER	= $(JSON_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
JSON_ENV	= YPSILON_SITELIB=$(JSON_LIBPATH):$(YPSILON_SITELIB)
JSON_RUNNER	= $(JSON_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
JSON_ENV		= LARCENY_LIBPATH=$(JSON_LIBPATH):$(LARCENY_LIBPATH)
JSON_RUNNER	= $(JSON_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: json

json:
	cd $(srcdir)/src/libraries/json && $(JSON_RUNNER) $(JSON_PROGRAM)


#page
## --------------------------------------------------------------------
## Special rules: uri lexers and parser building.
## --------------------------------------------------------------------

URI_PROGRAM	= make-tables.sps
URI_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
URI_ENV	= VICARE_LIBRARY_PATH=$(URI_LIBPATH):$(VICARE_LIBRARY_PATH)
URI_RUNNER	= $(URI_ENV) $(VICARE) --debug --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
URI_ENV		= IKARUS_LIBRARY_PATH=$(URI_LIBPATH):$(IKARUS_LIBRARY_PATH)
URI_RUNNER	= $(URI_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
URI_ENV		= MOSH_LOADPATH=$(URI_LIBPATH):$(MOSH_LOADPATH)
URI_RUNNER	= $(URI_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
URI_ENV		= PETITE_LIBPATH=$(URI_LIBPATH):$(PETITE_LIBPATH)
URI_RUNNER	= $(URI_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
URI_ENV	= YPSILON_SITELIB=$(URI_LIBPATH):$(YPSILON_SITELIB)
URI_RUNNER	= $(URI_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
URI_ENV		= LARCENY_LIBPATH=$(URI_LIBPATH):$(LARCENY_LIBPATH)
URI_RUNNER	= $(URI_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: uri

uri:
	cd $(srcdir)/src/libraries/uri && $(URI_RUNNER) $(URI_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: net-related lexers and parser building.
## --------------------------------------------------------------------

NET_PROGRAM	= make-tables.sps
NET_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
NET_ENV	= VICARE_LIBRARY_PATH=$(NET_LIBPATH):$(VICARE_LIBRARY_PATH)
NET_RUNNER	= $(NET_ENV) $(VICARE) --debug --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_IKARUS))
NET_ENV		= IKARUS_LIBRARY_PATH=$(NET_LIBPATH):$(IKARUS_LIBRARY_PATH)
NET_RUNNER	= $(NET_ENV) $(IKARUS) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
NET_ENV		= MOSH_LOADPATH=$(NET_LIBPATH):$(MOSH_LOADPATH)
NET_RUNNER	= $(NET_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
NET_ENV		= PETITE_LIBPATH=$(NET_LIBPATH):$(PETITE_LIBPATH)
NET_RUNNER	= $(NET_ENV) $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
NET_ENV	= YPSILON_SITELIB=$(NET_LIBPATH):$(YPSILON_SITELIB)
NET_RUNNER	= $(NET_ENV) $(YPSILON)
else ifeq (yes,$(nausicaa_ENABLE_LARCENY))
NET_ENV		= LARCENY_LIBPATH=$(NET_LIBPATH):$(LARCENY_LIBPATH)
NET_RUNNER	= $(NET_ENV) $(LARCENY) -r6rs -program
endif

.PHONY: net

net:
	cd $(srcdir)/src/libraries/net/helpers && $(NET_RUNNER) $(NET_PROGRAM)



### end of file
# Local Variables:
# mode: makefile-gmake
# End:
