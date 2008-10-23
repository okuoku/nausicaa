# Makefile --
#
# Installation rules for GNU Readline wrapper around Ikarus Scheme.

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
SED_FLAGS	= -e "s|@IKARUS_BINDIR@|$(IKARUS_BINDIR)|g" \
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

RM		= rm --force --verbose --
RMDIR		= rmdir --parents --ignore-fail-on-non-empty --

## ------------------------------------------------------------

#page
## ------------------------------------------------------------
## Rules.
## ------------------------------------------------------------

SCRIPTS		= ike
COMPLETIONS	= $(wildcard *_completions *_identifiers)

.PHONY: all clean install install-bin install-dat

all:
	$(SED) $(SED_FLAGS) < ike.in > ike

clean:
	-$(RM) ike

## ------------------------------------------------------------

install: install-bin install-data

install-bin:
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
	$(INSTALL_BIN) $(SCRIPTS) $(DESTDIR)$(bindir)

install-data:
	$(INSTALL_DIR) $(DESTDIR)$(pkgdatadir)
	$(INSTALL_DATA) $(COMPLETIONS) $(DESTDIR)$(pkgdatadir)

## ------------------------------------------------------------

uninstall: uninstall-bin uninstall-data

uninstall-bin:
	-$(RM) $(addprefix $(bindir)/,$(SCRIPTS))
	-$(RMDIR) $(bindir)

uninstall-data:
	-$(RM) $(addprefix $(pkgdatadir)/,$(COMPLETIONS))
	-$(RMDIR) $(pkgdatadir)

## ------------------------------------------------------------

.PHONY: abi abu

abi: all install

abu: all uninstall install

### end of file
