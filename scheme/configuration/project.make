#
# Part of: Nausicaa/Scheme
# Contents: project specific makefile
# Date: Wed Jan 21, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

ds_texi_AUX_PREREQ		= $(wildcard $(ds_texi_SRCDIR)/*.texiinc)

#page
## --------------------------------------------------------------------
## Installation of source and fasl libraries.
## --------------------------------------------------------------------

$(eval $(call nau-libraries,core,.))

# Lexicographic order please.
$(eval $(call nau-libraries,armor,nausicaa/armor))
$(eval $(call nau-libraries,bytevectors,nausicaa/bytevectors))
$(eval $(call nau-libraries,char_sets,nausicaa/char-sets))
$(eval $(call nau-libraries,cond_expand,nausicaa/language/cond-expand))
$(eval $(call nau-libraries,classes,nausicaa/language/classes))
$(eval $(call nau-libraries,contracts,nausicaa/contracts))
$(eval $(call nau-libraries,csv,nausicaa/csv))
$(eval $(call nau-libraries,email,nausicaa/email))
$(eval $(call nau-libraries,email_addresses,nausicaa/email/addresses))
$(eval $(call nau-libraries,evaluations,nausicaa/evaluations))
$(eval $(call nau-libraries,ffi,nausicaa/ffi))
$(eval $(call nau-libraries,ffi_clang,nausicaa/ffi/clang))
$(eval $(call nau-libraries,ffi_clang_type_translation,nausicaa/ffi/clang/type-translation))
$(eval $(call nau-libraries,ffi_pointers,nausicaa/ffi/pointers))
$(eval $(call nau-libraries,ffi_peekers_and_pokers,nausicaa/ffi/peekers-and-pokers))
$(eval $(call nau-libraries,ffi_memory,nausicaa/ffi/memory))
$(eval $(call nau-libraries,ffi_memory_alloc,nausicaa/ffi/memory/alloc))
$(eval $(call nau-libraries,generics,nausicaa/generics))
$(eval $(call nau-libraries,generics_language,nausicaa/language/generics))
$(eval $(call nau-libraries,identifier_properties,nausicaa/language/identifier-properties))
$(eval $(call nau-libraries,infix,nausicaa/infix))
$(eval $(call nau-libraries,interps,nausicaa/interps))
$(eval $(call nau-libraries,json,nausicaa/json))
$(eval $(call nau-libraries,lalr,nausicaa/lalr))
$(eval $(call nau-libraries,lists,nausicaa/lists))
$(eval $(call nau-libraries,makers,nausicaa/language/makers))
##$(eval $(call nau-libraries,msgcat,nausicaa/msgcat))
$(eval $(call nau-libraries,nausicaa,nausicaa))
$(eval $(call nau-libraries,nausicaa_language,nausicaa/language))
$(eval $(call nau-libraries,net,nausicaa/net))
$(eval $(call nau-libraries,net_helpers,nausicaa/net/helpers))
$(eval $(call nau-libraries,parser_tools,nausicaa/parser-tools))
$(eval $(call nau-libraries,profiling,nausicaa/profiling))
$(eval $(call nau-libraries,randomisations,nausicaa/randomisations))
$(eval $(call nau-libraries,r6rs,nausicaa/r6rs))
$(eval $(call nau-libraries,silex,nausicaa/silex))
$(eval $(call nau-libraries,strings,nausicaa/strings))
$(eval $(call nau-libraries,times_and_dates,nausicaa/times-and-dates))
$(eval $(call nau-libraries,uri,nausicaa/uri))
$(eval $(call nau-libraries,vectors,nausicaa/vectors))

#page
## --------------------------------------------------------------------
## Message catalogs.
## --------------------------------------------------------------------

$(eval $(call ds-srcdir,msgcat,$(srcdir)/src/libraries/nausicaa/msgcat))
$(eval $(call ds-builddir,msgcat,$(nau_sls_BUILDDIR)/nausicaa/msgcat))

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
	$(call ds-if-yes,$(nausicaa_ENABLE_YPSILON),	ypsilon-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_MOSH),	mosh-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_PETITE),	petite-scheme-script)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_VICARE),	vicare-scheme-script))
binfmt_scripts_TARGETS	= $(call ds-replace-dir,$(binfmt_scripts_BUILDDIR),$(binfmt_scripts_SOURCES))
binfmt_scripts_INSTLST	= $(binfmt_scripts_TARGETS)
binfmt_scripts_INSTDIR	= $(pkglibexecdir)

#$(call ds-if-yes,$(nausicaa_ENABLE_IKARUS),	ikarus-scheme-script)
#$(call ds-if-yes,$(nausicaa_ENABLE_LARCENY),	larceny-scheme-script)

$(eval $(call ds-default-clean-variables,binfmt_scripts))
$(eval $(call ds-module,binfmt_scripts,bin,BIN))

$(binfmt_scripts_TARGETS): $(binfmt_scripts_BUILDDIR)/% : $(binfmt_scripts_SRCDIR)/%
	$(SED) \
		-e 's%__MOSH__%$(MOSH)%g'		\
		-e 's%__PETITE__%$(PETITE)%g'		\
		-e 's%__YPSILON__%$(YPSILON)%g'		\
		-e 's%__VICARE__%$(VICARE)%g'		\
		<$(<) >$(@)

#-e 's%__IKARUS__%$(IKARUS)%g'
#-e 's%__LARCENY__%$(LARCENY)%g'

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
	-e 's%OPT_ENABLE_MOSH%$(nausicaa_ENABLE_MOSH)%g'	\
	-e 's%OPT_ENABLE_PETITE%$(nausicaa_ENABLE_PETITE)%g'	\
	-e 's%OPT_ENABLE_YPSILON%$(nausicaa_ENABLE_YPSILON)%g'	\
	-e 's%OPT_ENABLE_VICARE%$(nausicaa_ENABLE_VICARE)%g'	\
	<$(binfmt_config_SRCDIR)/rc.scheme			\
	>$(binfmt_config_BUILDDIR)/rc.scheme

#-e 's%OPT_ENABLE_IKARUS%$(nausicaa_ENABLE_IKARUS)%g'
#-e 's%OPT_ENABLE_LARCENY%$(nausicaa_ENABLE_LARCENY)%g'

endif

#page
## ------------------------------------------------------------
## R6RS compatibility tests.
## ------------------------------------------------------------

test_compat_SCRIPT	= $(srcdir)/tests/r6rs/run.sps

.PHONY: test-compat

## ------------------------------------------------------------

# ifeq ($(strip $(nausicaa_ENABLE_IKARUS)),yes)
# .PHONY: test-ikarus-compat

# test-ikarus-compat:
# 	IKARUS_LIBRARY_PATH=$(srcdir):$(IKARUS_LIBRARY_PATH) \
# 	$(IKARUS) --r6rs-script $(test_compat_SCRIPT)

# test-compat: test-ikarus-compat
# endif

## ------------------------------------------------------------

# ifeq ($(strip $(nausicaa_ENABLE_LARCENY)),yes)
# .PHONY: test-larceny-compat

# test-larceny-compat:
# 	LARCENY_LIBPATH=$(srcdir):$(LARCENY_LIBPATH) \
# 	$(LARCENY) -r6rs -program  $(test_compat_SCRIPT)

# test-compat: test-larceny-compat
# endif

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

ifeq ($(strip $(nausicaa_ENABLE_RACKET)),yes)
.PHONY: test-racket-compat

test-racket-compat:
	PLTCOLLECTS=$(PWD)/$(srcdir):$(PLTCOLLECTS) \
	$(RACKET) $(test_compat_SCRIPT)

test-compat: test-racket-compat
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
## Special test rules.
## --------------------------------------------------------------------

.PHONY: vtest-r6rs-parser ptest-r6rs-parser mtest-r6rs-parser

vtest-r6rs-parser:
	$(nau_vtest_RUN) $(srcdir)/tests/r6rs-parser-test.sps $(file)

ptest-r6rs-parser:
	$(nau_ptest_RUN) $(srcdir)/tests/r6rs-parser-test.sps $(file)

mtest-r6rs-parser:
	$(nau_mtest_RUN) $(srcdir)/tests/r6rs-parser-test.sps $(file)

#page
## --------------------------------------------------------------------
## Special rules: SILex lexers building.
## --------------------------------------------------------------------

SILEX_SRCDIR		= $(srcdir)/src/libraries/nausicaa/silex
SILEX_TEST_PROGRAMS	= $(wildcard $(srcdir)/tests/make-silex-*.sps)
SILEX_LIBPATH		= $(abspath $(srcdir)/tests):$(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
SILEX_ENV	= VICARE_LIBRARY_PATH=$(SILEX_LIBPATH):$(VICARE_LIBRARY_PATH)
SILEX_RUNNER	= $(SILEX_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
SILEX_ENV	= PETITE_LIBPATH=$(SILEX_LIBPATH):$(CHEZSCHEMELIBDIRS)
SILEX_RUNNER	= export $(SILEX_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
SILEX_ENV	= MOSH_LOADPATH=$(SILEX_LIBPATH):$(MOSH_LOADPATH)
SILEX_RUNNER	= $(SILEX_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
SILEX_ENV	= YPSILON_SITELIB=$(SILEX_LIBPATH):$(YPSILON_SITELIB)
SILEX_RUNNER	= $(SILEX_ENV) $(YPSILON)
endif

.PHONY: silex-internals silex-backup silex silex-test silex-custom

silex-internals:
	cd $(SILEX_SRCDIR) && $(SILEX_RUNNER) make-tables.sps

silex-backup:
	$(CP) $(SILEX_SRCDIR)/*-l.sls $(SILEX_SRCDIR)/backup

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
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
LALR_ENV	= PETITE_LIBPATH=$(LALR_LIBPATH):$(CHEZSCHEMELIBDIRS)
LALR_RUNNER	= export $(LALR_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
LALR_ENV	= MOSH_LOADPATH=$(LALR_LIBPATH):$(MOSH_LOADPATH)
LALR_RUNNER	= $(LALR_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
LALR_ENV	= YPSILON_SITELIB=$(LALR_LIBPATH):$(YPSILON_SITELIB)
LALR_RUNNER	= $(LALR_ENV) $(YPSILON)
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
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
CSV_ENV		= PETITE_LIBPATH=$(CSV_LIBPATH):$(CHEZSCHEMELIBDIRS)
CSV_RUNNER	= export $(CSV_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
CSV_ENV		= MOSH_LOADPATH=$(CSV_LIBPATH):$(MOSH_LOADPATH)
CSV_RUNNER	= $(CSV_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
CSV_ENV		= YPSILON_SITELIB=$(CSV_LIBPATH):$(YPSILON_SITELIB)
CSV_RUNNER	= $(CSV_ENV) $(YPSILON)
endif

.PHONY: csv

csv:
	cd $(srcdir)/src/libraries/nausicaa/csv && $(CSV_RUNNER) $(CSV_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: Infix lexer and parser building.
## --------------------------------------------------------------------

INFIX_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
INFIX_ENV	= VICARE_LIBRARY_PATH=$(INFIX_LIBPATH):$(VICARE_LIBRARY_PATH)
INFIX_RUNNER	= $(INFIX_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
INFIX_ENV	= PETITE_LIBPATH=$(INFIX_LIBPATH):$(CHEZSCHEMELIBDIRS)
INFIX_RUNNER	= export $(INFIX_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
INFIX_ENV	= MOSH_LOADPATH=$(INFIX_LIBPATH):$(MOSH_LOADPATH)
INFIX_RUNNER	= $(INFIX_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
INFIX_ENV	= YPSILON_SITELIB=$(INFIX_LIBPATH):$(YPSILON_SITELIB)
INFIX_RUNNER	= $(INFIX_ENV) $(YPSILON)
endif

.PHONY: infix

infix:
	cd $(srcdir)/src/libraries/nausicaa/infix && $(INFIX_RUNNER) make-tables.sps

#page
## --------------------------------------------------------------------
## Special rules: email address lexers building.
## --------------------------------------------------------------------

EMAIL_PROGRAM	= make-tables.sps
EMAIL_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
EMAIL_ENV	= VICARE_LIBRARY_PATH=$(EMAIL_LIBPATH):$(VICARE_LIBRARY_PATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(VICARE) --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
EMAIL_ENV	= PETITE_LIBPATH=$(EMAIL_LIBPATH):$(CHEZSCHEMELIBDIRS)
EMAIL_RUNNER	= export $(EMAIL_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
EMAIL_ENV	= MOSH_LOADPATH=$(EMAIL_LIBPATH):$(MOSH_LOADPATH)
EMAIL_RUNNER	= $(EMAIL_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
EMAIL_ENV	= YPSILON_SITELIB=$(EMAIL_LIBPATH):$(YPSILON_SITELIB)
EMAIL_RUNNER	= $(EMAIL_ENV) $(YPSILON)
endif

.PHONY: email

email:
	cd $(srcdir)/src/libraries/nausicaa/email/addresses && $(EMAIL_RUNNER) $(EMAIL_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: json lexers and parser building.
## --------------------------------------------------------------------

JSON_PROGRAM	= make-tables.sps
JSON_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
JSON_ENV	= VICARE_LIBRARY_PATH=$(JSON_LIBPATH):$(VICARE_LIBRARY_PATH)
JSON_RUNNER	= $(JSON_ENV) $(VICARE) --debug --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
JSON_ENV	= PETITE_LIBPATH=$(JSON_LIBPATH):$(CHEZSCHEMELIBDIRS)
JSON_RUNNER	= export $(JSON_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
JSON_ENV	= MOSH_LOADPATH=$(JSON_LIBPATH):$(MOSH_LOADPATH)
JSON_RUNNER	= $(JSON_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
JSON_ENV	= YPSILON_SITELIB=$(JSON_LIBPATH):$(YPSILON_SITELIB)
JSON_RUNNER	= $(JSON_ENV) $(YPSILON)
endif

.PHONY: json

json:
	cd $(srcdir)/src/libraries/nausicaa/json && $(JSON_RUNNER) $(JSON_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: net-related lexers and parser building.
## --------------------------------------------------------------------

NET_PROGRAM	= make-tables.sps
NET_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
NET_ENV		= VICARE_LIBRARY_PATH=$(NET_LIBPATH):$(VICARE_LIBRARY_PATH)
NET_RUNNER	= $(NET_ENV) $(VICARE) --debug --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
NET_ENV		= MOSH_LOADPATH=$(NET_LIBPATH):$(MOSH_LOADPATH)
NET_RUNNER	= $(NET_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
NET_ENV		= PETITE_LIBPATH=$(NET_LIBPATH):$(CHEZSCHEMELIBDIRS)
NET_RUNNER	= export $(NET_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
NET_ENV		= YPSILON_SITELIB=$(NET_LIBPATH):$(YPSILON_SITELIB)
NET_RUNNER	= $(NET_ENV) $(YPSILON)
endif

.PHONY: net

net:
	cd $(srcdir)/src/libraries/nausicaa/net/helpers && $(NET_RUNNER) $(NET_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: R6RS lexer and parser building.
## --------------------------------------------------------------------

R6RS_LEXER_PROGRAM	= make-tables.sps
R6RS_LEXER_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_PETITE))
R6RS_LEXER_ENV		= PETITE_LIBPATH=$(R6RS_LEXER_LIBPATH):$(CHEZSCHEMELIBDIRS)
R6RS_LEXER_RUNNER	= export $(R6RS_LEXER_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_VICARE))
R6RS_LEXER_ENV		= VICARE_LIBRARY_PATH=$(R6RS_LEXER_LIBPATH):$(VICARE_LIBRARY_PATH)
R6RS_LEXER_RUNNER	= $(R6RS_LEXER_ENV) $(VICARE) --debug --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
R6RS_LEXER_ENV		= MOSH_LOADPATH=$(R6RS_LEXER_LIBPATH):$(MOSH_LOADPATH)
R6RS_LEXER_RUNNER	= $(R6RS_LEXER_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
R6RS_LEXER_ENV		= YPSILON_SITELIB=$(R6RS_LEXER_LIBPATH):$(YPSILON_SITELIB)
R6RS_LEXER_RUNNER	= $(R6RS_LEXER_ENV) $(YPSILON)
endif

.PHONY: R6RS_LEXER

r6rs-lexer:
	cd $(srcdir)/src/libraries/nausicaa/r6rs && $(R6RS_LEXER_RUNNER) $(R6RS_LEXER_PROGRAM)

#page
## --------------------------------------------------------------------
## Special rules: XML lexer and parser building.
## --------------------------------------------------------------------

XML_LEXER_PROGRAM	= make-tables.sps
XML_LEXER_LIBPATH	= $(abspath $(nau_sls_BUILDDIR))

ifeq (yes,$(nausicaa_ENABLE_VICARE))
XML_LEXER_ENV		= VICARE_LIBRARY_PATH=$(XML_LEXER_LIBPATH):$(VICARE_LIBRARY_PATH)
XML_LEXER_RUNNER	= $(XML_LEXER_ENV) $(VICARE) --debug --r6rs-script
else ifeq (yes,$(nausicaa_ENABLE_PETITE))
XML_LEXER_ENV		= PETITE_LIBPATH=$(XML_LEXER_LIBPATH):$(CHEZSCHEMELIBDIRS)
XML_LEXER_RUNNER	= export $(XML_LEXER_ENV); $(PETITE) --libdirs $${PETITE_LIBPATH} --program
else ifeq (yes,$(nausicaa_ENABLE_MOSH))
XML_LEXER_ENV		= MOSH_LOADPATH=$(XML_LEXER_LIBPATH):$(MOSH_LOADPATH)
XML_LEXER_RUNNER	= $(XML_LEXER_ENV) $(MOSH)
else ifeq (yes,$(nausicaa_ENABLE_YPSILON))
XML_LEXER_ENV		= YPSILON_SITELIB=$(XML_LEXER_LIBPATH):$(YPSILON_SITELIB)
XML_LEXER_RUNNER	= $(XML_LEXER_ENV) $(YPSILON)
endif

.PHONY: XML_LEXER

xml-lexer:
	cd $(srcdir)/src/libraries/nausicaa/xml/markups && $(XML_LEXER_RUNNER) $(XML_LEXER_PROGRAM)

#page
## --------------------------------------------------------------------
## Rebuiding all SILex tables.
## --------------------------------------------------------------------

.PHONY:	silex-all-tables

silex-all-tables:
	$(MAKE) silex
	$(MAKE) lalr
	$(MAKE) csv
	$(MAKE) infix
	$(MAKE) email
	$(MAKE) json
	$(MAKE) net
	$(MAKE) r6rs-lexer
#	$(MAKE) xml-lexer

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
