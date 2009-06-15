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
## Library source files.
## ---------------------------------------------------------------------

ifeq ($(nausicaa_ENABLE_SLS),yes)
$(eval $(call ds-srcdir,sls,$(srcdir)/src/libraries))
$(eval $(call ds-builddir,sls,$(builddir)/fasl.d))

sls_FILES	= $(shell cd $(sls_SRCDIR) ; $(FIND) -type f -name \*.sls)
sls_SOURCES	= $(addprefix $(sls_SRCDIR)/,$(sls_FILES))
sls_TARGETS	= $(addprefix $(sls_BUILDDIR)/,$(sls_FILES))
sls_INSTLST	= $(sls_TARGETS)
sls_INSTDIR	= $(pkglibdir)

$(eval $(call ds-default-clean-variables,sls))
$(eval $(call ds-module,sls,bin))

$(sls_TARGETS): $(sls_BUILDDIR)/%: $(sls_SRCDIR)/%
	@test -d $(dir $(@)) || $(MKDIR) $(dir $(@))
	@$(CP) $(<) $(@)

endif # nausicaa_ENABLE_SLS == yes

## ---------------------------------------------------------------------

ifeq ($(nausicaa_ENABLE_FASL),yes)
$(eval $(call ds-srcdir,fasl,$(sls_BUILDDIR)))
$(eval $(call ds-builddir,fasl,$(sls_BUILDDIR)))

fasl_ikarus_TARGETS	= $(call ds-if-yes,$(nausicaa_ENABLE_IKARUS),\
	$(addprefix $(sls_BUILDDIR)/,$(shell cd $(sls_BUILDDIR) && $(FIND) -name \*.ikarus*fasl)))
fasl_mosh_TARGETS	= $(call ds-if-yes,$(nausicaa_ENABLE_MOSH),\
	$(addprefix $(sls_BUILDDIR)/,$(shell cd $(sls_BUILDDIR) && $(FIND) -name \*.mosh*fasl)))
fasl_larceny_TARGETS	= $(call ds-if-yes,$(nausicaa_ENABLE_LARCENY),\
	$(addprefix $(sls_BUILDDIR)/,$(shell cd $(sls_BUILDDIR) && $(FIND) -name \*.slfasl)))
fasl_TARGETS	=
fasl_INSTLST	= $(fasl_ikarus_TARGETS) $(fasl_mosh_TARGETS) $(fasl_larceny_TARGETS)
fasl_INSTDIR	= $(pkglibdir)

fasl_CLEANFILES		= $(fasl_ikarus_TARGETS) $(fasl_larceny_TARGETS) $(fasl_mosh_TARGETS)
fasl_MOSTLYLCLEANFILES	= $(fasl_CLEANFILES)

$(eval $(call ds-module,fasl,bin))

fasl-all:	$(call ds-if-yes,$(nausicaa_ENABLE_IKARUS),	ifasl)	\
		$(call ds-if-yes,$(nausicaa_ENABLE_LARCENY),	lfasl)	\
		$(call ds-if-yes,$(nausicaa_ENABLE_MOSH),	mfasl)

endif # nausicaa_ENABLE_FASL == yes

## --------------------------------------------------------------------

fasl_ikarus_COMPILE_SCRIPT	= $(sls_SRCDIR)/compile-all.ikarus.sps
fasl_ikarus_COMPILE_ENV		= IKARUS_LIBRARY_PATH=$(sls_BUILDDIR):$(IKARUS_LIBRARY_PATH) IKARUS_FASL_DIRECTORY=/
fasl_ikarus_COMPILE_COMMAND	= $(IKARUS) --compile-dependencies
fasl_ikarus_COMPILE_RUN		= $(fasl_ikarus_COMPILE_ENV) $(fasl_ikarus_COMPILE_COMMAND) $(fasl_ikarus_COMPILE_SCRIPT)

.PHONY: ifasl ifasl-clean

ifasl: sls-all ifasl-clean
	test -f $(fasl_ikarus_COMPILE_SCRIPT) && $(fasl_ikarus_COMPILE_RUN)

ifasl-clean:
	$(RM) $(fasl_ikarus_TARGETS)

## ---------------------------------------------------------------------

fasl_mosh_COMPILE_SCRIPT	= $(sls_SRCDIR)/compile-all.mosh.sps
ifeq (,$(strip $(MOSH_LOADPATH)))
fasl_mosh_COMPILE_ENV		= MOSH_LOADPATH=$(sls_BUILDDIR)
else
fasl_mosh_COMPILE_ENV		= MOSH_LOADPATH=$(sls_BUILDDIR):$(MOSH_LOADPATH)
endif
fasl_mosh_COMPILE_COMMAND	= printf \
	"(import (rnrs)(mosh))\n ((symbol-value 'pre-compile-r6rs-file) \"%s\")\n(exit)\n" \
	$(fasl_mosh_COMPILE_SCRIPT) | $(fasl_mosh_COMPILE_ENV) $(MOSH)
fasl_mosh_COMPILE_RUN		= $(fasl_mosh_COMPILE_COMMAND)

.PHONY: mfasl mfasl-clean

mfasl: sls-all mfasl-clean
# Commented out Wed Jun  3, 2009 because compiling is still unstable.
#	test -f $(fasl_mosh_COMPILE_SCRIPT) && $(fasl_mosh_COMPILE_RUN)

mfasl-clean:
	$(RM) $(fasl_mosh_TARGETS)

## ---------------------------------------------------------------------

fasl_larceny_COMPILE_SCRIPT	= $(PWD)/$(sls_SRCDIR)/compile-all.larceny.sps
fasl_larceny_COMPILE_ENV	= LARCENY_LIBPATH=$(PWD)/$(sls_BUILDDIR):$(LARCENY_LIBPATH)
fasl_larceny_COMPILE_COMMAND	= $(LARCENY) -r6rs -program
fasl_larceny_COMPILE_RUN	= $(fasl_larceny_COMPILE_ENV) $(fasl_larceny_COMPILE_COMMAND) $(fasl_larceny_COMPILE_SCRIPT)

.PHONY: lfasl lfasl-clean

lfasl: sls-all lfasl-clean
	test -f $(fasl_larceny_COMPILE_SCRIPT) && (cd $(sls_BUILDDIR) && $(fasl_larceny_COMPILE_RUN))

lfasl-clean:
	$(RM) $(fasl_larceny_TARGETS)

#page
## ---------------------------------------------------------------------
## Testing.
## ---------------------------------------------------------------------

ifdef LIBPATH
THE_LIBPATH		= $(LIBPATH):
endif

## ---------------------------------------------------------------------

nau_test_SRCDIR		= $(srcdir)/tests
nau_test_FILES		= $(wildcard $(nau_test_SRCDIR)/test-*.sps)
nau_test_SELECTED_FILES	= $(wildcard $(nau_test_SRCDIR)/test-*$(file)*.sps)

ifneq ($(strip $(name)),)
nau_test_ENV		+= CHECK_TEST_NAME=$(name)
endif

# Here we include the build directory but not the source directory.
nau_test_PATH		= $(THE_LIBPATH)$(sls_BUILDDIR):$(srcdir)/tests

.PHONY: tests test check

tests test check:

## ---------------------------------------------------------------------

nau_itest_ENV		= IKARUS_LIBRARY_PATH=$(nau_test_PATH):$(IKARUS_LIBRARY_PATH)
nau_itest_ENV		+= $(nau_test_ENV)
nau_itest_PROGRAM	= $(IKARUS) --r6rs-script
#nau_itest_PROGRAM	= $(IKARUS) --debug --r6rs-script
nau_itest_RUN		= $(nau_itest_ENV) $(nau_itest_PROGRAM)

.PHONY: itests itest icheck

itests itest icheck:
ifeq ($(strip $(nausicaa_ENABLE_IKARUS)),yes)
ifeq ($(strip $(file)),)
	$(foreach f,$(nau_test_FILES),$(nau_itest_RUN) $(f);)
else
	$(foreach f,$(nau_test_SELECTED_FILES),$(nau_itest_RUN) $(f);)
endif

tests test check: itest
endif

## ---------------------------------------------------------------------

nau_ltest_ENV		= LARCENY_LIBPATH=$(nau_test_PATH):$(LARCENY_LIBPATH)
nau_ltest_ENV		+= $(nau_test_ENV)
nau_ltest_PROGRAM	= $(LARCENY) -r6rs -program
nau_ltest_RUN		= $(nau_ltest_ENV) $(nau_ltest_PROGRAM)

.PHONY: ltests ltest lcheck

ltests ltest lcheck:
ifeq ($(strip $(nausicaa_ENABLE_LARCENY)),yes)
ifeq ($(strip $(file)),)
	$(foreach f,$(nau_test_FILES),$(nau_ltest_RUN) $(f);)
else
	$(foreach f,$(nau_test_SELECTED_FILES),$(nau_ltest_RUN) $(f);)
endif

tests test check: ltest
endif

## ------------------------------------------------------------

ifeq (,$(strip $(MOSH_LOADPATH)))
nau_mtest_ENV		= MOSH_LOADPATH=$(nau_test_PATH)
else
nau_mtest_ENV		= MOSH_LOADPATH=$(nau_test_PATH):$(MOSH_LOADPATH)
endif
nau_mtest_ENV		+= $(nau_test_ENV)
nau_mtest_PROGRAM	= $(MOSH)
nau_mtest_RUN		= $(nau_mtest_ENV) $(nau_mtest_PROGRAM)

.PHONY: mtests mtest mcheck

mtests mtest mcheck:
ifeq ($(strip $(nausicaa_ENABLE_MOSH)),yes)
ifeq ($(strip $(file)),)
	$(foreach f,$(nau_test_FILES),$(nau_mtest_RUN) $(f);)
else
	$(foreach f,$(nau_test_SELECTED_FILES),$(nau_mtest_RUN) $(f);)
endif

tests test check: mtest
endif

## ---------------------------------------------------------------------

nau_ytest_ENV		= YPSILON_SITELIB=$(nau_test_PATH):$(YPSILON_SITELIB)
nau_ytest_ENV		+= $(nau_test_ENV)
nau_ytest_PROGRAM	= $(YPSILON) --r6rs --warning --compatible
nau_ytest_RUN		= $(nau_ytest_ENV) $(nau_ytest_PROGRAM)

.PHONY: ytests ytest ycheck

ytests ytest ycheck:
ifeq ($(strip $(nausicaa_ENABLE_YPSILON)),yes)
ifeq ($(strip $(file)),)
	$(foreach f,$(nau_test_FILES),$(nau_ytest_RUN) $(f);)
else
	$(foreach f,$(nau_test_SELECTED_FILES),$(nau_ytest_RUN) $(f);)
endif

tests test check: ytest
endif


### end of file
# Local Variables:
# mode: makefile-gmake
# End:
