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
$(eval $(call nau-libraries,nausicaa,nausicaa))
$(eval $(call nau-libraries,strings,strings))
$(eval $(call nau-libraries,vectors,vectors))

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


### end of file
# Local Variables:
# mode: makefile-gmake
# End:
