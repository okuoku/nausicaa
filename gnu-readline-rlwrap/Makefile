# Makefile --
#
# Installation rules for GNU Readline wrapper around Ikarus Scheme.

#page
## ------------------------------------------------------------
## Global variables.
## ------------------------------------------------------------

srcdir		= $(PWD)
builddir	= "$(PWD)/=build"

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Data.
## ------------------------------------------------------------

# The list  of completion  modules to include.   Each string
# must be the prefix of a '*_completions' file in the source
# directory.
MODULES		= ikarus ffi					\
				srfi_and_let_star		\
				srfi_args_fold			\
				srfi_case_lambda		\
				srfi_cond_expand		\
				srfi_eager_comp			\
				srfi_error_reporting		\
				srfi_general_cond		\
				srfi_let_values 		\
				srfi_lightweight_testing	\
				srfi_list			\
				srfi_parameters			\
				srfi_random			\
				srfi_rec			\
				srfi_receive			\
				srfi_records			\
				srfi_specialize_procedures	\
				srfi_streams			\
				srfi_string_ports		\
				srfi_strings			\
				srfi_time			\
				srfi_vector


## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Wrapper installation directories.
## ------------------------------------------------------------

prefix		= $(HOME)
bindir		= $(prefix)/bin
datadir		= $(prefix)/share
#pkgdatadir	= $(datadir)/ikarus
pkgdatadir	= $(prefix)

ifeq ($(strip $(prefix)),)
prefix		= ~
endif

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Ikarus installation directories.
## ------------------------------------------------------------

ikarus_PREFIX		= /usr/local
ikarus_BINDIR		= ${ikarus_PREFIX}/bin
ikarus_LIBDIR		= ${ikarus_PREFIX}/lib
ikarus_PKGLIBDIR	= ${ikarus_LIBDIR}/ikarus

ypsilon_PREFIX		= /usr/local
ypsilon_BINDIR		= ${ypsilon_PREFIX}/bin
ypsilon_LIBDIR		= ${ypsilon_PREFIX}/lib
ypsilon_PKGLIBDIR	= ${ypsilon_LIBDIR}/ypsilon

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Programs.
## ------------------------------------------------------------

SED		= sed
IKARUS_SED_FLAGS	= \
	-e "s|@IKARUS_PREFIX@|$(ikarus_PREFIX)|g"	\
	-e "s|@IKARUS_BINDIR@|$(ikarus_BINDIR)|g"	\
	-e "s|@IKARUS_LIBDIR@|$(ikarus_LIBDIR)|g"	\
	-e "s|@IKARUS_PKGLIBDIR@|$(ikarus_PKGLIBDIR)|g"	\
	-e "s|@prefix@|$(prefix)|g"			\
	-e "s|@bindir@|$(bindir)|g"			\
	-e "s|@datadir@|$(datadir)|g"			\
	-e "s|@pkgdatadir@|$(pkgdatadir)|g"

YPSILON_SED_FLAGS	= \
	-e "s|@YPSILON_PREFIX@|$(ypsilon_PREFIX)|g"	\
	-e "s|@YPSILON_BINDIR@|$(ypsilon_BINDIR)|g"	\
	-e "s|@YPSILON_LIBDIR@|$(ypsilon_LIBDIR)|g"	\
	-e "s|@YPSILON_PKGLIBDIR@|$(ypsilon_PKGLIBDIR)|g"\
	-e "s|@prefix@|$(prefix)|g"			\
	-e "s|@bindir@|$(bindir)|g"			\
	-e "s|@datadir@|$(datadir)|g"			\
	-e "s|@pkgdatadir@|$(pkgdatadir)|g"

## ------------------------------------------------------------

INSTALL			= install

INSTALL_DIR_MODE	= 0755
INSTALL_BIN_MODE	= 0555
INSTALL_DATA_MODE	= 0444

INSTALL_DIR		= $(INSTALL) -m $(INSTALL_DIR_MODE) -d
INSTALL_BIN		= $(INSTALL) -m $(INSTALL_BIN_MODE)
INSTALL_DATA		= $(INSTALL) -m $(INSTALL_DATA_MODE)

## ------------------------------------------------------------

RM		= rm --force --recursive --verbose --
RMDIR		= rmdir --parents --ignore-fail-on-non-empty --
MKDIR		= mkdir --parents --verbose

SORT		= sort --unique

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Rules.
## ------------------------------------------------------------

COMPLETIONS	= r6rs_identifiers \
		  $(wildcard $(foreach m,$(MODULES),$(m)_completions))

SCRIPTS		= $(builddir)/ike $(builddir)/ype
ikarus_COMPFILE	= $(builddir)/.ikarus_completions
ypsilon_COMPFILE= $(builddir)/.ypsilon_completions
COMPFILES	= $(ikarus_COMPFILE) $(ypsilon_COMPFILE)

.PHONY: all clean install install-bin install-dat

all: builddir
	$(SED) $(IKARUS_SED_FLAGS)  < ike.in > $(builddir)/ike
	$(SED) $(YPSILON_SED_FLAGS) < ype.in > $(builddir)/ype
	cat $(COMPLETIONS) | $(SORT) >$(ikarus_COMPFILE)
	cat $(COMPLETIONS) | $(SORT) >$(ypsilon_COMPFILE)

clean:
	-$(RM) $(builddir)

builddir:
	-@test -d $(builddir) || $(MKDIR) $(builddir)

## ------------------------------------------------------------

install: install-bin install-data

install-bin:
	test -d $(DESTDIR)$(bindir) || $(INSTALL_DIR) $(DESTDIR)$(bindir)
	$(INSTALL_BIN) $(SCRIPTS) $(DESTDIR)$(bindir)

install-data:
	test -d $(DESTDIR)$(pkgdatadir) || $(INSTALL_DIR) $(DESTDIR)$(pkgdatadir)
	$(INSTALL_DATA) $(COMPFILES) $(DESTDIR)$(pkgdatadir)

## ------------------------------------------------------------

uninstall: uninstall-bin uninstall-data

uninstall-bin:
	-$(RM) $(addprefix $(DESTDIR)$(bindir)/,$(notdir $(SCRIPTS)))
	-$(RMDIR) $(DESTDIR)$(bindir)

uninstall-data:
	-$(RM) $(addprefix $(DESTDIR)$(pkgdatadir)/,$(notdir $(COMPFILES)))
	-$(RMDIR) $(DESTDIR)$(pkgdatadir)

## ------------------------------------------------------------

.PHONY: abi abu

abi: all install

abu: all uninstall install

### end of file
