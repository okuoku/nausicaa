# @configure_input@
#
# Part of: Nausicaa
# Contents: Nausicaa specific infrastructure
# Date: Fri Mar 27, 2009
#
# Abstract
#
#
#
# Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
## --------------------------------------------------------------------
## Configuration.
## --------------------------------------------------------------------

nausicaa_ENABLE_SLS		= @nausicaa_ENABLE_SLS@
nausicaa_ENABLE_FASL		= @nausicaa_ENABLE_FASL@
nausicaa_ENABLE_IKARUS		= @nausicaa_ENABLE_IKARUS@
nausicaa_ENABLE_LARCENY		= @nausicaa_ENABLE_LARCENY@
nausicaa_ENABLE_MOSH		= @nausicaa_ENABLE_MOSH@
nausicaa_ENABLE_YPSILON		= @nausicaa_ENABLE_YPSILON@

FIND		= @FIND@
IKARUS		= @IKARUS@
LARCENY		= @LARCENY@
MOSH		= @MOSH@
YPSILON		= @YPSILON@

#page
## ---------------------------------------------------------------------
## Installation of library source files.
## ---------------------------------------------------------------------

# $(1) - the identifier
# $(2) - the subdirectory
define nau-sls-libraries
ifeq ($$(nausicaa_ENABLE_SLS),yes)

$$(eval $$(call ds-srcdir,nau_sls_$(1),$$(srcdir)/src/libraries/$(2)))

nau_sls_$(1)_SOURCES	= $$(call ds-glob,nau_sls_$(1),*.sls)
nau_sls_$(1)_INSTLST	= $$(nau_sls_$(1)_SOURCES)
nau_sls_$(1)_INSTDIR	= $$(pkglibdir)/$(2)

$$(eval $$(call ds-module,nau_sls_$(1),bin))

endif # nausicaa_ENABLE_SLS == yes
endef

## --------------------------------------------------------------------

# $(1) - the identifier
# $(2) - the subdirectory
define nau-fasl-libraries
ifeq ($(nausicaa_ENABLE_FASL),yes)

$$(eval $$(call ds-srcdir,nau_fasl_$(1),$(builddir)/fasl.d/$(2)))

nau_fasl_$(1)_PATTERNS	= \
	$(call ds-if-yes,$(nausicaa_ENABLE_IKARUS),	*.ikarus*fasl)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_LARCENY),	*.slfasl)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_MOSH),	*.mosh*fasl)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_YPSILON),	*.cache)

nau_fasl_$(1)_SOURCES	= $$(call ds-glob,nau_fasl_$(1),$$(nau_fasl_$(1)_PATTERNS))
nau_fasl_$(1)_INSTLST	= $$(nau_fasl_$(1)_SOURCES)
nau_fasl_$(1)_INSTDIR	= $$(pkglibdir)/$(2)

$$(eval $$(call ds-module,nau_fasl_$(1),bin))

endif # nausicaa_ENABLE_FASL == yes
endef

## --------------------------------------------------------------------

# $(1) - the identifier
# $(2) - the subdirectory
define nau-libraries
$$(eval $$(call nau-sls-libraries,$(1),$(2)))
$$(eval $$(call nau-fasl-libraries,$(1),$(2)))
endef

#page
## --------------------------------------------------------------------
## General compiled files rules.
## --------------------------------------------------------------------

ifeq ($(nausicaa_ENABLE_FASL),yes)

nau_IMPLEMENTATIONS	= \
	$(call ds-if-yes,$(nausicaa_ENABLE_IKARUS),	ifasl)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_LARCENY),	lfasl)	\
	$(call ds-if-yes,$(nausicaa_ENABLE_MOSH),	mfasl)  \
	$(call ds-if-yes,$(nausicaa_ENABLE_YPSILON),	yfasl)

$(eval $(call ds-srcdir,fasl,$(srcdir)/src/libraries))
$(eval $(call ds-builddir,fasl,$(builddir)/fasl.d))

fasl_SOURCES	= $(shell cd $(fasl_SRCDIR) ; $(FIND) -type f -name \*.sls)
fasl_TARGETS	= $(addprefix $(fasl_BUILDDIR)/,$(fasl_SOURCES))

$(fasl_TARGETS): $(fasl_BUILDDIR)/%: $(fasl_SRCDIR)/%
	@test -d $(dir $(@)) || $(MKDIR) $(dir $(@))
	@$(CP) $(<) $(@)

fasl: $(nau_IMPLEMENTATIONS)

fasl-clean: $(foreach i,$(nau_IMPLEMENTATIONS),$(i)-clean)
	$(RM) $(fasl_BUILDDIR)

bin:		fasl
bin-clean:	fasl-clean

endif # nausicaa_ENABLE_FASL == yes

#page
## --------------------------------------------------------------------
## Ikarus compilation.
## --------------------------------------------------------------------

ifeq (yes,$(nausicaa_ENABLE_FASL))
ifeq (yes,$(nausicaa_ENABLE_IKARUS))

fasl_ikarus_FASL	= $(shell cd $(fasl_BUILDDIR) && $(FIND) -name \*.ikarus*fasl)
fasl_ikarus_TARGETS	= $(addprefix $(fasl_BUILDDIR)/,$(fasl_ikarus_FASL))

fasl_ikarus_COMPILE_SCRIPT	= $(fasl_SRCDIR)/compile-all.ikarus.sps
fasl_ikarus_COMPILE_ENV		= IKARUS_LIBRARY_PATH=$(fasl_BUILDDIR):$(IKARUS_LIBRARY_PATH) \
				  IKARUS_FASL_DIRECTORY=/
fasl_ikarus_COMPILE_COMMAND	= $(IKARUS) --compile-dependencies
fasl_ikarus_COMPILE_RUN		= $(fasl_ikarus_COMPILE_ENV) \
					$(fasl_ikarus_COMPILE_COMMAND) \
					$(fasl_ikarus_COMPILE_SCRIPT)

.PHONY: ifasl ifasl-clean

ifasl: $(fasl_TARGETS) ifasl-clean
	test -f $(fasl_ikarus_COMPILE_SCRIPT) && $(fasl_ikarus_COMPILE_RUN)

ifasl-clean:
	$(RM) $(fasl_ikarus_TARGETS)

endif # nausicaa_ENABLE_IKARUS == yes
endif # nausicaa_ENABLE_FASL == yes

#page
## --------------------------------------------------------------------
## Mosh compilation.
## --------------------------------------------------------------------

ifeq (yes,$(nausicaa_ENABLE_FASL))
ifeq (yes,$(nausicaa_ENABLE_MOSH))

fasl_mosh_FASL		= $(shell cd $(fasl_BUILDDIR) && $(FIND) -name \*.mosh*fasl)
fasl_mosh_TARGETS	= $(addprefix $(fasl_BUILDDIR)/,$(fasl_mosh_FASL))

fasl_mosh_COMPILE_SCRIPT	= $(fasl_SRCDIR)/compile-all.mosh.sps
ifeq (,$(strip $(MOSH_LOADPATH)))
fasl_mosh_COMPILE_ENV		= MOSH_LOADPATH=$(fasl_BUILDDIR)
else
fasl_mosh_COMPILE_ENV		= MOSH_LOADPATH=$(fasl_BUILDDIR):$(MOSH_LOADPATH)
endif
fasl_mosh_COMPILE_COMMAND	= printf \
	"(import (rnrs)(mosh))\n ((symbol-value 'pre-compile-r6rs-file) \"%s\")\n(exit)\n" \
	$(fasl_mosh_COMPILE_SCRIPT) | $(fasl_mosh_COMPILE_ENV) $(MOSH)
fasl_mosh_COMPILE_RUN		= $(fasl_mosh_COMPILE_COMMAND)

.PHONY: mfasl mfasl-clean

mfasl: $(fasl_TARGETS) mfasl-clean
# Commented out Wed Jun  3, 2009 because compiling is still unstable.
#	test -f $(fasl_mosh_COMPILE_SCRIPT) && $(fasl_mosh_COMPILE_RUN)

mfasl-clean:
	$(RM) $(fasl_mosh_TARGETS)

endif # nausicaa_ENABLE_MOSH == yes
endif # nausicaa_ENABLE_FASL == yes

#page
## --------------------------------------------------------------------
## Larceny compilation.
## --------------------------------------------------------------------

ifeq (yes,$(nausicaa_ENABLE_FASL))
ifeq (yes,$(nausicaa_ENABLE_LARCENY))

fasl_larceny_FASL	= $(shell cd $(fasl_BUILDDIR) && $(FIND) -name \*.slfasl)
fasl_larceny_TARGETS	= $(addprefix $(fasl_BUILDDIR)/,$(fasl_larceny_FASL))

# The  use of  ABSPATH  is needed  because  we change  directory in  the
# compile commands below.
fasl_larceny_COMPILE_SCRIPT	= $(abspath $(fasl_SRCDIR)/compile-all.larceny.sps)
fasl_larceny_COMPILE_ENV	= LARCENY_LIBPATH=$(PWD)/$(fasl_BUILDDIR):$(LARCENY_LIBPATH)
fasl_larceny_COMPILE_COMMAND	= $(LARCENY) -r6rs -program
fasl_larceny_COMPILE_RUN	= $(fasl_larceny_COMPILE_ENV) \
					$(fasl_larceny_COMPILE_COMMAND) \
					$(fasl_larceny_COMPILE_SCRIPT)

.PHONY: lfasl lfasl-clean

lfasl: $(fasl_TARGETS) lfasl-clean
	test -f $(fasl_larceny_COMPILE_SCRIPT) && (cd $(fasl_BUILDDIR) && $(fasl_larceny_COMPILE_RUN))

lfasl-clean:
	$(RM) $(fasl_larceny_TARGETS)

endif # nausicaa_ENABLE_MOSH == yes
endif # nausicaa_ENABLE_FASL == yes

#page
## --------------------------------------------------------------------
## Ypsilon compilation.
## --------------------------------------------------------------------

ifeq (yes,$(nausicaa_ENABLE_FASL))
ifeq (yes,$(nausicaa_ENABLE_YPSILON))

.PHONY: yfasl yfasl-clean

yfasl: $(fasl_TARGETS) yfasl-clean

yfasl-clean:

endif # nausicaa_ENABLE_MOSH == yes
endif # nausicaa_ENABLE_FASL == yes

#page
## ---------------------------------------------------------------------
## Testing.
## ---------------------------------------------------------------------

# Enable timing of test files  execution.  We cannot include this in the
# separator  because  it needs  to  be  placed  between the  environment
# variables and the actual command line.
nausicaa_TIME_TESTS	= @nausicaa_TIME_TESTS@
ifeq (yes,$(strip $(nausicaa_TIME_TESTS)))
nau_TIME_TESTS		= time -p
endif

nau_test_SEPARATOR	= echo;echo "===> test file $(2) with $(1)";echo;

## --------------------------------------------------------------------

# The variable  "file" is available to  the user on the  command line of
# "make": It selects specific files.
nau_test_SRCDIR		= $(srcdir)/tests
ifneq (,$(strip $(file)))
nau_test_FILES		= $(wildcard $(nau_test_SRCDIR)/test-*$(file)*.sps)
else
nau_test_FILES		= $(wildcard $(nau_test_SRCDIR)/test-*.sps)
endif

# The variable  "name" is available to  the user on the  command line of
# "make": It selects specific tests.
ifneq ($(strip $(name)),)
nau_test_ENV		+= CHECK_TEST_NAME=$(name)
endif

# Here we include the build directory but not the source directory.  The
# variable "LIBPATH"  is available  to the user  on the command  line of
# "make": It prepends more directories to the search path.
ifdef LIBPATH
nau_test_custom_LIBPATH	= $(LIBPATH):
endif
nau_test_PATH		= $(nau_test_custom_LIBPATH)$(fasl_BUILDDIR):$(srcdir)/tests

.PHONY: tests test check

tests test check:

## ---------------------------------------------------------------------
## Ikarus

nau_itest_ENV		= IKARUS_LIBRARY_PATH=$(nau_test_PATH):$(IKARUS_LIBRARY_PATH)
nau_itest_ENV		+= $(nau_test_ENV)
nau_itest_PROGRAM	= $(IKARUS) --r6rs-script
#nau_itest_PROGRAM	= $(IKARUS) --debug --r6rs-script
nau_itest_RUN		= $(nau_itest_ENV) $(nau_TIME_TESTS) $(nau_itest_PROGRAM)

.PHONY: itest itests icheck

itest itests icheck:
ifeq ($(strip $(nausicaa_ENABLE_IKARUS)),yes)
	@$(foreach f,$(nau_test_FILES),\
		$(call nau_test_SEPARATOR,Ikarus,$(f)) $(nau_itest_RUN) $(f))
endif

test tests check: itest

## ---------------------------------------------------------------------
## Larceny

nau_ltest_ENV		= LARCENY_LIBPATH=$(nau_test_PATH):$(LARCENY_LIBPATH)
nau_ltest_ENV		+= $(nau_test_ENV)
nau_ltest_PROGRAM	= $(LARCENY) -r6rs -program
nau_ltest_RUN		= $(nau_ltest_ENV) $(nau_TIME_TESTS) $(nau_ltest_PROGRAM)

.PHONY: ltest ltests lcheck

ltest ltests lcheck:
ifeq ($(strip $(nausicaa_ENABLE_LARCENY)),yes)
	@$(foreach f,$(nau_test_FILES),\
		$(call nau_test_SEPARATOR,Larceny,$(f)) $(nau_ltest_RUN) $(f))
endif

test tests check: ltest

## ------------------------------------------------------------
## Mosh

ifeq (,$(strip $(MOSH_LOADPATH)))
nau_mtest_ENV		= MOSH_LOADPATH=$(nau_test_PATH)
else
nau_mtest_ENV		= MOSH_LOADPATH=$(nau_test_PATH):$(MOSH_LOADPATH)
endif
nau_mtest_ENV		+= $(nau_test_ENV)
nau_mtest_PROGRAM	= $(MOSH)
nau_mtest_RUN		= $(nau_mtest_ENV) $(nau_TIME_TESTS) $(nau_mtest_PROGRAM)

.PHONY: mtest mtests mcheck

mtest mtests mcheck:
ifeq ($(strip $(nausicaa_ENABLE_MOSH)),yes)
	@$(foreach f,$(nau_test_FILES),\
		$(call nau_test_SEPARATOR,Mosh,$(f)) $(nau_mtest_RUN) $(f))
endif

test tests check: mtest

## ---------------------------------------------------------------------
## Ypsilon

nau_ytest_ENV		= YPSILON_SITELIB=$(nau_test_PATH):$(YPSILON_SITELIB)
nau_ytest_ENV		+= $(nau_test_ENV)
nau_ytest_PROGRAM	= $(YPSILON) --r6rs --warning --compatible
nau_ytest_RUN		= $(nau_ytest_ENV) $(nau_TIME_TESTS) $(nau_ytest_PROGRAM)

.PHONY: ytest ytests ycheck

ytest ytests ycheck:
ifeq ($(strip $(nausicaa_ENABLE_YPSILON)),yes)
	@$(foreach f,$(nau_test_FILES),\
		$(call nau_test_SEPARATOR,Ypsilon,$(f)) $(nau_ytest_RUN) $(f))
endif

test tests check: ytest

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
