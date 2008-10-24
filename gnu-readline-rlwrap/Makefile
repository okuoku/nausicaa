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
## Customisation.
## ------------------------------------------------------------

# Customise this file for the installation directories.
include Makefile.custom

ifeq ($(strip $(prefix)),)
HOME	= ~
endif

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Programs.
## ------------------------------------------------------------

SED		= sed
SED_FLAGS	= \
	-e "s|@IKARUS_PREFIX@|$(IKARUS_PREFIX)|g"	\
	-e "s|@IKARUS_BINDIR@|$(IKARUS_BINDIR)|g"	\
	-e "s|@IKARUS_LIBDIR@|$(IKARUS_LIBDIR)|g"	\
	-e "s|@IKARUS_PKGLIBDIR@|$(IKARUS_PKGLIBDIR)|g"	\
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

SORT		= sort

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Rules.
## ------------------------------------------------------------

COMPLETIONS	= r6rs_identifiers \
		  $(wildcard $(foreach m,$(MODULES),$(m)_completions))

SCRIPT		= $(builddir)/ike
COMPFILE	= $(builddir)/.ikarus_completions

.PHONY: all clean install install-bin install-dat

all: builddir
	$(SED) $(SED_FLAGS) < ike.in > $(builddir)/ike
	cat $(COMPLETIONS) | sort >$(COMPFILE)

clean:
	-$(RM) $(builddir)

builddir:
	-@test -d $(builddir) || $(MKDIR) $(builddir)

## ------------------------------------------------------------

install: install-bin install-data

install-bin:
	test -d $(DESTDIR)$(bindir) || $(INSTALL_DIR) $(DESTDIR)$(bindir)
	$(INSTALL_BIN) $(SCRIPT) $(DESTDIR)$(bindir)

install-data:
	test -d $(DESTDIR)$(pkgdatadir) || $(INSTALL_DIR) $(DESTDIR)$(pkgdatadir)
	$(INSTALL_DATA) $(COMPFILE) $(DESTDIR)$(pkgdatadir)

## ------------------------------------------------------------

uninstall: uninstall-bin uninstall-data

uninstall-bin:
	-$(RM) $(addprefix $(DESTDIR)$(bindir)/,$(notdir $(SCRIPT)))
	-$(RMDIR) $(DESTDIR)$(bindir)

uninstall-data:
	-$(RM) $(addprefix $(DESTDIR)$(pkgdatadir)/,$(notdir $(COMPFILE)))
	-$(RMDIR) $(DESTDIR)$(pkgdatadir)

## ------------------------------------------------------------

.PHONY: abi abu

abi: all install

abu: all uninstall install

### end of file
