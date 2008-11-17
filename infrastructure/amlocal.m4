divert(-1)dnl
dnl 
dnl Part of: DevelStuff
dnl Contents: library of Makefile blocks
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
dnl Setup.
dnl --------------------------------------------------------------------

changequote([,])


dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Helper macros.
dnl --------------------------------------------------------------------

define([DS_COMMENT_SECTION],[#page
## ------------------------------------------------------------
## $1
## ------------------------------------------------------------])

define([DS_COMMENT_SEPARATOR],
[## ------------------------------------------------------------])

define([DS_BLOCK],[DS_COMMENT_SECTION([$1])
$2
DS_COMMENT_SEPARATOR 
]) 

define([DS_DEFINE_BLOCK],[define([$1],[DS_BLOCK([$2],[$3])])])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Configuration variables.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_CONFIGURATION_VARIABLES],
		[Configuration variables.],[
# If set to 'yes' documentation files will be produced and
# installed.
ds_config_ENABLE_DOC		= @ds_config_ENABLE_DOC@

# If set to 'yes' Info documentation files will be produced
# and installed.
ds_config_ENABLE_INFO_DOC	= @ds_config_ENABLE_DOC_INFO@

# If set to 'yes' HTML documentation files will be produced
# and installed.
ds_config_ENABLE_HTML_DOC	= @ds_config_ENABLE_DOC_HTML@

# Compressor to be used when creating a tarball; it is used
# by the binary distribution rules.  Supported values:
#
#   bzip2		selects the Bzip2 compressor
#   gzip		selects the Gzip compressor
#
# if a different value is set, the compressor will default
# to 'gzip'.
# 
ds_config_COMPRESSOR		?= gzip
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Binary package configuration variables.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_BINARY_PACKAGE_VARIABLES],
		[Binary package distribtion building variables.],[
# The variable 'package_BUILD_VERSION'  should be defined in
# "Makefile.custom".  If  not we set a default  that does no
# harm.
ifeq ($(strip $(package_BUILD_VERSION)),)
package_BUILD_VERSION	= 1
endif

package_NAME		= $(PACKAGE_NAME)
package_VERSION		= $(PACKAGE_VERSION)

package_ARCH		= \
	$(firstword $(subst -, ,$(shell ../../infrastructure/config.guess)))
ifeq ($(strip $(package_ARCH)),)
package_ARCH		= noarch
endif

package_PREFIX		= \
$(package_NAME)-$(package_VERSION)-$(package_ARCH)-$(package_BUILD_VERSION)
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Directories.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_DIRECTORIES],[Directories.],[
ifeq ($(strip $(TMPDIR)),)
TMPDIR		= /tmp
endif

## ------------------------------------------------------------

# The  other modules  will assume  that this  is  a relative
# directory  pathname, and  the full  pathname can  be built
# with: '$(PWD)/$(srcdir)'.
srcdir			= @srcdir@

# The  other modules  will assume  that this  is  a relative
# directory  pathname, and  the full  pathname can  be built
# with: '$(PWD)/$(builddir)'.
builddir		= @builddir@

## ------------------------------------------------------------

prefix			= @prefix@
exec_prefix		= @exec_prefix@

bindir			= @bindir@
datarootdir		= @datarootdir@
datadir			= @datadir@
docdir			= @datarootdir@/doc
htmldir			= @htmldir@
includedir		= @includedir@
infodir			= @infodir@
libdir			= @libdir@
libexecdir		= @libexecdir@
localstatedir		= @localstatedir@
mandir			= @mandir@
sbindir			= @sbindir@
sharedstatedir		= @sharedstatedir@
sysconfdir		= @sysconfdir@

PKG_DIR			= @PKG_DIR@
pkgdatadir		= @pkgdatadir@
pkgdocdir		= @pkgdocdir@
pkgexampledir		= @pkgexampledir@
pkginfodir		= @pkginfodir@
pkghtmldir		= @pkghtmldir@
pkgincludedir		= @pkgincludedir@
pkglibdir		= @pkglibdir@
pkglibexecdir		= @pkglibexecdir@
pkgsysconfdir		= @pkgsysconfdir@
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Programs.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_PROGRAMS],[Common programs.],[
BASH_PROGRAM	= @BASH_PROGRAM@
SHELL		= @SHELL@
@SET_MAKE@

BZIP		= @BZIP@
CAT		= @CAT@
CP		= @CP@ --force --verbose --preserve=mode --
FIND		= @FIND@
GREP		= @GREP@
GAWK		= @GAWK@
GZIP		= @GZIP@
M4		= @M4@
MAKEINFO	= @MAKEINFO@
MKDIR		= @MKDIR@ --parents --verbose
MV		= @MV@ --verbose --
RM		= @RM@ --force --recursive --verbose --
RM_FILE		= @RM@ --force --verbose --
RM_SILENT	= @RM@ --force --recursive --
RMDIR		= @RMDIR@ --parents --ignore-fail-on-non-empty --
SED		= @SED@
SORT		= @SORT@
SUDO		= @SUDO@
SYMLINK		= @SYMLINK@ --symbolic
TAR		= @TAR@

DS_COMMENT_SEPARATOR

INSTALL			= @INSTALL@
INSTALL_DIR_MODE	?= 0755
INSTALL_BIN_MODE	?= 0555
INSTALL_DATA_MODE	?= 0444
INSTALL_LIB_MODE	?= 0444

INSTALL_DIR		= $(INSTALL) -p -m $(INSTALL_DIR_MODE) -d
INSTALL_BIN		= $(INSTALL) -p -m $(INSTALL_BIN_MODE)
INSTALL_DATA		= $(INSTALL) -p -m $(INSTALL_DATA_MODE)

DS_COMMENT_SEPARATOR

MAKEINFO_FLAGS		= --no-split -I$(doc_texinfo_SRCDIR) -I$(doc_texinfo_BUILDDIR)
MAKEINFO_INFO_FLAGS	= $(MAKEINFO_FLAGS)
MAKEINFO_HTML_FLAGS	= $(MAKEINFO_FLAGS) --html
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Master phony rules.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_MAIN_PHONY_RULES],[Main phony rules.],[
.PHONY: all
.PHONY: clean realclean clean-builddir
.PHONY: install uninstall
.PHONY: doc doc-clean doc-install doc-uninstall

all:
clean:
realclean:	clean
install:
uninstall:

clean-builddir:
	-@printf '*** The build directory is: %s\n' $(builddir)
	-@read -p '*** Are you sure to clean it? (yes/no) ' ANSWER; \
	test "$${ANSWER}" = yes && $(RM) $(builddir)/*

DS_COMMENT_SEPARATOR

doc:
doc-clean:
doc-install:
doc-uninstall:

all:		doc
clean:		doc-clean
install:	doc-install
uninstall:	doc-uninstall
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Install instructions.
dnl --------------------------------------------------------------------

dnl Synopsis:
dnl 
dnl   DS_INSTALL_FILE(<1 FILE-PATHNAME>,<2 TARGET-DIRECTORY>,<3 MODE>)
dnl 
dnl Arguments:
dnl 
dnl   <FILE-PATHNAME>
dnl	The pathname  of the  file to install,  relative to  the current
dnl	directory.
dnl
dnl   <TARGET-DIRECTORY>
dnl     The full pathname  of the installation directory.
dnl
dnl   <MODE>
dnl     The installation mode of the file.
dnl 
dnl Description:
dnl 
dnl   Insert  the  command required  to  install  a  file in  the  given
dnl directory.
dnl
dnl Usage example:
dnl
dnl   DS_INSTALL_FILE($(builddir)/liba.so,$(libdir),$(INSTALL_DATA_MODE))
dnl
define([DS_INSTALL_FILE],[	$(INSTALL) -m $3 $1 $(DESTDIR)$2])

dnl Synopsis:
dnl 
dnl   DS_INSTALL_DIR(<1 TARGET-DIRECTORY>,<2 MODE>)
dnl 
dnl Arguments:
dnl 
dnl   <TARGET-DIRECTORY>
dnl     The full pathname of directory to install.
dnl
dnl   <MODE>
dnl     The installation mode of the directory.
dnl 
dnl Description:
dnl 
dnl  Insert the command required to install a directory.
dnl
dnl Usage example:
dnl
dnl   DS_INSTALL_DIR($(libdir),$(INSTALL_DIR_MODE))
dnl
define([DS_INSTALL_DIR],[	$(INSTALL) -m $2 -d $(DESTDIR)$1])

dnl Synopsis:
dnl 
dnl   DS_INSTALL_MODULE(<PREFIX>,<DIR_MODE>,<FILE_MODE>)
dnl 
dnl Arguments:
dnl 
dnl   .
dnl 
dnl Description:
dnl 
dnl   .
dnl
dnl Usage examples:
dnl
dnl   .
dnl
define([DS_INSTALL_MODULE],[dnl
DS_INSTALL_DIR($($1_INSTDIR),$2)dnl
foreach([X],(),[DS_INSTALL_FILE(X,$($1_INSTDIR),$3)])dnl
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Documentation: Texinfo format main rules.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_DOC_TEXINFO],
		[Documentation: Texinfo format main rules.],[
# It is allright if the package has no documentation in
# Texinfo format.   The rules below will take  care of doing
# nothing if no .texi files are found.

doc_texinfo_SRCDIR	= $(srcdir)/doc
doc_texinfo_BUILDDIR	= $(builddir)/doc-texinfo.d

doc_texinfo_SOURCES	= $(wildcard $(doc_texinfo_SRCDIR)/*.texi)
doc_texinfo_TARGETS	= $(doc_texinfo_info_TARGETS) $(doc_texinfo_html_TARGETS)
doc_texinfo_CLEANFILES	= $(doc_texinfo_TARGETS)

DS_COMMENT_SEPARATOR
 
.PHONY: doc-texinfo doc-texinfo-clean doc-texinfo-builddir
.PHONY: doc-texinfo-install doc-texinfo-uninstall doc-texinfo-version

ifeq ($(strip $(ds_config_ENABLE_DOC)),yes)
doc-texinfo:		doc-texinfo-info          doc-texinfo-html
doc-texinfo-install:	doc-texinfo-info-install  doc-texinfo-html-install
doc-texinfo-uninstall:	doc-texinfo-info-uinstall doc-texinfo-html-uinstall
else
doc-texinfo doc-texinfo-install doc-texinfo-uninstall:
endif
 
doc-texinfo-builddir:
	-@test -d $(doc_texinfo_BUILDDIR) || $(MKDIR) $(doc_texinfo_BUILDDIR)

doc-texinfo-clean:
	-$(RM) $(doc_texinfo_CLEANFILES)

doc-texinfo-version: $(doc_texinfo_BUILDDIR)/version.texiinc

$(doc_texinfo_BUILDDIR)/version.texiinc:
	printf '@macro version{}\n%s\n@end macro' $(PACKAGE_VERSION) >$(@)

doc-texinfo-clean doc-texinfo-builddir:

DS_COMMENT_SEPARATOR

doc:		doc-texinfo
doc-clean:	doc-texinfo-clean
doc-install:	doc-texinfo-install
doc-uninstall:	doc-texinfo-uninstall
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Documentation: Texinfo format, Info output.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_DOC_TEXINFO_INFO],
		[Documentation: Texinfo format Info rules.],[
doc_texinfo_info_TARGETS= $(foreach f,$(doc_texinfo_SOURCES:.texi=.info),\
			  $(addprefix $(doc_texinfo_BUILDDIR)/,$(notdir $(f))))

doc_texinfo_info_INSTLST= $(doc_texinfo_info_TARGETS)
doc_texinfo_info_INSTDIR= $(infodir)

DS_COMMENT_SEPARATOR

.PHONY: doc-texinfo-info doc-texinfo-info-install doc-texinfo-info-uninstall

ifeq ($(strip $(ds_config_ENABLE_INFO_DOC)),yes)

doc-texinfo-info: doc-texinfo-builddir doc-texinfo-version $(doc_texinfo_info_TARGETS)

$(doc_texinfo_info_TARGETS): $(doc_texinfo_BUILDDIR)/%.info : $(doc_texinfo_SRCDIR)/%.texi
	$(MAKEINFO) $(MAKEINFO_INFO_FLAGS) -o $(@) $(<)

doc-texinfo-info-install:
ifneq ($(strip $(wildcard $(doc_texinfo_info_INSTLST))),)
	$(INSTALL_DIR) $(DESTDIR)$(doc_texinfo_info_INSTDIR)
	$(INSTALL_DATA) $(doc_texinfo_info_INSTLST) $(DESTDIR)$(doc_texinfo_info_INSTDIR)
endif

doc-texinfo-info-uninstall:
ifneq ($(strip $(wildcard $(doc_texinfo_info_INSTLST))),)
	-@$(RM) $(addprefix $(DESTDIR)$(doc_texinfo_info_INSTDIR)/,$(foreach f,$(doc_texinfo_info_INSTLST),$(notdir $(f))))
	-@$(RMDIR) $(DESTDIR)$(doc_texinfo_info_INSTDIR)
endif

endif		# ds_config_ENABLE_INFO_DOC == yes
])

dnl --------------------------------------------------------------------
 
dnl page
dnl --------------------------------------------------------------------
dnl Documentation: Texinfo format, HTML output.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_DOC_TEXINFO_HTML],
		[Documentation: Texinfo format HTML rules.],[
doc_texinfo_html_TARGETS= $(foreach f,$(doc_texinfo_SOURCES:.texi=.html),\
			  $(addprefix $(doc_texinfo_BUILDDIR)/,$(notdir $(f))))

doc_texinfo_html_INSTLST= $(doc_texinfo_html_TARGETS)
doc_texinfo_html_INSTDIR= $(pkghtmldir)

DS_COMMENT_SEPARATOR

.PHONY: doc-texinfo-html doc-texinfo-html-install doc-texinfo-html-uninstall

ifeq ($(strip $(ds_config_ENABLE_HTML_DOC)),yes)

doc-texinfo-html: doc-texinfo-builddir doc-texinfo-version $(doc_texinfo_html_TARGETS)

$(doc_texinfo_html_TARGETS): $(doc_texinfo_BUILDDIR)/%.html : $(doc_texinfo_SRCDIR)/%.texi
	$(MAKEINFO) $(MAKEINFO_HTML_FLAGS) -o $(@) $(<)

doc-texinfo-html-install:
ifneq ($(strip $(wildcard $(doc_texinfo_html_INSTLST))),)
	$(INSTALL_DIR) $(DESTDIR)$(doc_texinfo_html_INSTDIR)
	$(INSTALL_DATA) $(doc_texinfo_html_INSTLST) $(DESTDIR)$(doc_texinfo_html_INSTDIR)
endif

doc-texinfo-html-uninstall:
ifneq ($(strip $(wildcard $(doc_texinfo_html_INSTLST))),)
	-@$(RM) $(addprefix $(DESTDIR)$(doc_texinfo_html_INSTDIR)/,$(foreach f,$(doc_texinfo_html_INSTLST),$(notdir $(f))))
	-@$(RMDIR) $(DESTDIR)$(doc_texinfo_html_INSTDIR)
endif

endif		# ds_config_ENABLE_HTML_DOC == yes
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Binary distribution.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_BINARY_DISTRIBUTION],[Binary distribution.],[
ifeq ($(strip $(nausicaa_COMPRESSOR)),bzip2)
bindist_PACKAGE_EXTENSION	= .tar.bz2
bindist_TAR_COMPRESS_FLAG	= --bzip2
else
bindist_PACKAGE_EXTENSION	= .tar.gz
bindist_TAR_COMPRESS_FLAG	= --gzip
endif

# This is the file  name of the binary distribution tarball.
# Notice that there is no directory part.
bindist_PACKAGE_NAME	= $(package_PREFIX)$(bindist_PACKAGE_EXTENSION)

# This is  the root directory for  temporary installation of
# files
bindist_PACKAGE_TOP_BUILDDIR	= $(TMPDIR)/$(PKG_ID)

# This is where the produced tarball will be finally stored.
bindist_BUILDDIR		= $(PWD)/$(builddir)/bindist.d

# This is the full pathname of the tarball.
bindist_TARBALL_PATHNAME	= $(bindist_BUILDDIR)/$(bindist_PACKAGE_NAME)

# The flags to hand to "tar" when building the tarball.
bindist_TAR_FLAGS		= --create $(bindist_TAR_COMPRESS_FLAG) --verbose

# By executing this command we get a list of the files in
# the bindist tarball.
bindist_LIST	= $(TAR) --list $(bindist_TAR_COMPRESS_FLAG) --file=$(bindist_TARBALL_PATHNAME)

DS_COMMENT_SEPARATOR

.PHONY: bindist bindist-clean bindist-builddir bindist-top-builddir
.PHONY: bindist-clean-top-builddir bindist-clean-builddir

bindist: bindist-clean bindist-builddir
	$(MAKE) install DESTDIR=$(bindist_PACKAGE_TOP_BUILDDIR)
	cd $(bindist_PACKAGE_TOP_BUILDDIR); \
	$(TAR) $(bindist_TAR_FLAGS) --file=$(bindist_TARBALL_PATHNAME) .
	$(MAKE) bindist-clean-top-builddir

bindist-clean: bindist-clean-top-builddir bindist-clean-builddir

bindist-top-builddir:
	test -d $(bindist_PACKAGE_TOP_BUILDDIR) || $(MKDIR) $(bindist_PACKAGE_TOP_BUILDDIR)

bindist-clean-top-builddir:
	-$(RM) $(bindist_PACKAGE_TOP_BUILDDIR)

bindist-builddir:
	test -d $(bindist_BUILDDIR) || $(MKDIR) $(bindist_BUILDDIR)

bindist-clean-builddir:
	-$(RM) $(bindist_BUILDDIR)
])

dnl --------------------------------------------------------------------
 
dnl page
dnl --------------------------------------------------------------------
dnl Slackware packaging.
dnl --------------------------------------------------------------------

define([DS_SLACKWARE_PACKAGING],[dnl
DS_SLACKWARE_PACKAGING_VARIABLES
DS_SLACKWARE_PACKAGING_PRIVATE_RULES
DS_SLACKWARE_PACKAGING_AUXILIARY_RULES
DS_SLACKWARE_PACKAGING_STANDARD_RULES
DS_SLACKWARE_PACKAGING_LOCAL_RULES
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Slackware packaging: variables.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_SLACKWARE_PACKAGING_VARIABLES],
		[Slackware packaging: common variables.],[
# This is  the file name  of the Slackware  package.  Notice
# that there is no directory part.
slack_PACKAGE_NAME	= $(package_PREFIX).tgz

# This  is the  package prefix,  it  is used  to remove  the
# package.
slack_PACKAGE_PREFIX	= $(package_PREFIX)

# This is  the root directory for  temporary installation of
# files.
slack_PACKAGE_TOP_BUILDDIR	= $(TMPDIR)/$(PKG_ID)

# This  is the  root directory  for building  packages.  The
# rules for standard packages will set it to:
#
#   $(slack_PACKAGE_TOP_BUILDDIR)
#
# the rules for local packages will set it to:
#
#   $(slack_PACKAGE_TOP_BUILDDIR)/$(prefix)
#
slack_PACKAGE_BUILDDIR	?=

# This is where the produced package will be finally stored.
slack_BUILDDIR		= $(PWD)/$(builddir)/slackware.d

# This is  the pathname of the  Slackware packages registry.
# The standard package rules will set it to:
#
#   $(slack_REGISTRY_DIR)
#
# while the local package rules will set it to:
#
#   $(prefix)$(slack_REGISTRY_DIR)
#
slack_REGISTRY		?= 
slack_REGISTRY_DIR	= /var/log/packages

# Try  to  read from  the  system  the  name of  an  already
# installed package.  It is used to upgrade.
slack_INSTALLED_PACKAGE	= \
	$(firstword $(notdir $(wildcard $(slack_REGISTRY)/$(package_NAME)-*)))

# This is the environment for the Slackware package handling
# tools.
slack_ENV		?=

slack_MAKEPKG_PROGRAM	= @slack_MAKEPKG_PROGRAM@
slack_MAKEPKG_FLAGS	?= --chown y
slack_MAKEPKG		= $(slack_ENV) $(slack_SUDO) $(slack_MAKEPKG_PROGRAM) $(slack_MAKEPKG_FLAGS)

slack_INSTALLPKG_PROGRAM= @slack_INSTALLPKG_PROGRAM@
slack_INSTALLPKG_FLAGS	?=
slack_INSTALLPKG	= $(slack_ENV) $(slack_SUDO) $(slack_INSTALLPKG_PROGRAM) $(slack_INSTALLPKG_FLAGS)

slack_REMOVEPKG_PROGRAM	= @slack_REMOVEPKG_PROGRAM@
slack_REMOVEPKG_FLAGS	?=
slack_REMOVEPKG		= $(slack_ENV) $(slack_SUDO) $(slack_REMOVEPKG_PROGRAM) $(slack_REMOVEPKG_FLAGS)

slack_UPGRADEPKG_PROGRAM= @slack_UPGRADEPKG_PROGRAM@
slack_UPGRADEPKG_FLAGS	?= --verbose --reinstall
slack_UPGRADEPKG	= $(slack_ENV) $(slack_SUDO) $(slack_UPGRADEPKG_PROGRAM) $(slack_UPGRADEPKG_FLAGS)
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Slackware packaging: private rules.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_SLACKWARE_PACKAGING_PRIVATE_RULES],
		[Slackware packaging: private building rules.],[
# The following rules are meant  to be invoked only by other
# rules.
#
# Notice  that,  by  default,  GNU tar  preserves  the  file
# modification times  so the FASL  times will be  newer than
# the corresponding .sls times in the Slackware package.

.PHONY: private-slackware        private-slackware-install
.PHONY: private-slackware-remove private-slackware-upgrade

private-slackware: slackware-clean slackware-builddir
	$(MAKE) install DESTDIR=$(slack_PACKAGE_TOP_BUILDDIR)
	$(INSTALL_DIR) $(slack_PACKAGE_BUILDDIR)/install
	$(INSTALL_DATA) $(builddir)/meta.d/slackware/slack-desc $(slack_PACKAGE_BUILDDIR)/install
	$(MAKE) slackware-aux
	cd $(slack_PACKAGE_BUILDDIR); \
	$(slack_MAKEPKG) $(slack_PACKAGE_NAME); \
	$(slack_SUDO) $(MV) $(slack_PACKAGE_NAME) $(slack_BUILDDIR)

private-slackware-install:
	cd $(slack_BUILDDIR); \
	$(slack_INSTALLPKG) $(slack_PACKAGE_NAME)

private-slackware-remove:
	$(slack_REMOVEPKG) $(slack_PACKAGE_PREFIX)

private-slackware-upgrade:
	cd $(slack_BUILDDIR); \
	$(slack_UPGRADEPKG) $(slack_INSTALLED_PACKAGE)%$(slack_PACKAGE_NAME)
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Slackware packaging: auxiliary rules.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_SLACKWARE_PACKAGING_AUXILIARY_RULES],
		[Slackware packaging: auxiliary rules.],[
.PHONY: slackware-builddir slackware-aux

slackware-builddir:
	-test -d $(slack_BUILDDIR) || $(MKDIR) $(slack_BUILDDIR)

slackware-top-builddir:
	-test -d $(slack_PACKAGE_TOP_BUILDDIR) || $(MKDIR) $(slack_PACKAGE_TOP_BUILDDIR)

# This is for auxiliary rules: it is freely available to the
# user.  It  will be  invoked just before  running "makepkg"
# (see the 'slackware' rule).
slackware-aux:

## ------------------------------------------------------------

.PHONY: slackware-clean slackware-clean-top-builddir slackware-clean-builddir

slackware-clean: slackware-clean-top-builddir slackware-clean-builddir

slackware-clean-top-builddir:
	-$(slack_SUDO) $(RM) $(slack_PACKAGE_TOP_BUILDDIR)

slackware-clean-builddir:
	-$(slack_SUDO) $(RM) $(slack_BUILDDIR)
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Slackware packaging: standard package rules.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_SLACKWARE_PACKAGING_STANDARD_RULES],
		[Slackware packaging: standard package build.],[
slackware_STANDARD_ENV	= \
	PATH=/sbin:$(PATH)					\
	slack_PACKAGE_BUILDDIR=$(slack_PACKAGE_TOP_BUILDDIR)	\
	slack_REGISTRY=$(slack_REGISTRY_DIR)

DS_COMMENT_SEPARATOR

.PHONY: slackware        slackware-install
.PHONY: slackware-remove slackware-upgrade

slackware:
	$(MAKE) private-slackware		$(slackware_STANDARD_ENV)
	$(MAKE) slackware-clean-top-builddir	$(slackware_STANDARD_ENV)

slackware-install:
	$(MAKE) private-slackware-install	$(slackware_STANDARD_ENV)

slackware-remove:
	$(MAKE) private-slackware-remove	$(slackware_STANDARD_ENV)

slackware-upgrade:
	$(MAKE) private-slackware-upgrade	$(slackware_STANDARD_ENV)
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Slackware packaging: local package rules.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_SLACKWARE_PACKAGING_LOCAL_RULES],
		[Slackware packaging: local package build.],[
slackware_LOCAL_ENV	= \
	PATH=/sbin:$(PATH)						\
	slack_PACKAGE_BUILDDIR=$(slack_PACKAGE_TOP_BUILDDIR)/$(prefix)	\
	slack_REGISTRY=$(prefix)$(slack_REGISTRY_DIR)			\
	slack_ENV=ROOT=$(prefix)

DS_COMMENT_SEPARATOR

.PHONY: local-slackware        local-slackware-install
.PHONY: local-slackware-remove local-slackware-upgrade

local-slackware:
	$(MAKE) private-slackware		$(slackware_LOCAL_ENV)
	$(MAKE) slackware-clean-top-builddir	$(slackware_LOCAL_ENV)

local-slackware-install:
	$(MAKE) private-slackware-install	$(slackware_LOCAL_ENV)

local-slackware-remove:
	$(MAKE) private-slackware-remove	$(slackware_LOCAL_ENV)

local-slackware-upgrade:
	$(MAKE) private-slackware-upgrade	$(slackware_LOCAL_ENV)

DS_COMMENT_SEPARATOR

.PHONY: abi abu

abi:
	$(MAKE) all
	$(MAKE) local-slackware local-slackware-install slack_SUDO=$(SUDO)
abu:
	$(MAKE) all
	$(MAKE) local-slackware local-slackware-upgrade slack_SUDO=$(SUDO)

])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl Redhat packaging.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_REDHAT_PACKAGING],
		[RedHat packaging: common variables.],[
# This is the file name  of the RedHat package.  Notice that
# there is no directory part.
redhat_PACKAGE_NAME	= $(package_PREFIX).rpm

# This  is the  package prefix,  it  is used  to remove  the
# package.
redhat_PACKAGE_PREFIX	= $(package_PREFIX)

# This is  the root directory for  temporary installation of
# files.
redhat_PACKAGE_TOP_BUILDDIR	= $(TMPDIR)/$(PKG_ID)

# This is the root directory for building packages.
redhat_PACKAGE_BUILDDIR	?= $(redhat_PACKAGE_TOP_BUILDDIR)/BUILD

# This is where the produced package will be finally stored.
redhat_BUILDDIR		= $(PWD)/$(builddir)/redhat.d

# Try  to  read from  the  system  the  name of  an  already
# installed package.  It is used to upgrade.
redhat_INSTALLED_PACKAGE	=

DS_COMMENT_SEPARATOR

# This is the environment for the RedHat package handling
# tools.
redhat_ENV		?=

redhat_CORE_PROGRAM	= @redhat_CORE_PROGRAM@

redhat_BUILD_PROGRAM	= @redhat_BUILD_PROGRAM@
redhat_BUILD_FLAGS	= -v --buildroot $(redhat_PACKAGE_BUILDDIR) -bb
redhat_BUILD		= $(redhat_ENV) $(redhat_BUILD_PROGRAM) $(redhat_BUILD_FLAGS)


redhat_SPEC_FILE	= $(PKG_ID).spec
redhat_GENERIC_SPEC	= meta.d/redhat/spec-file
redhat_SPECIFIC_SPEC	= meta.d/redhat/$(redhat_SPEC_FILE)

DS_COMMENT_SEPARATOR

.PHONY: redhat

redhat: bindist redhat-clean
	$(INSTALL_DIR) $(redhat_PACKAGE_TOP_BUILDDIR)
	$(INSTALL_DIR) $(redhat_PACKAGE_TOP_BUILDDIR)/BUILD
	$(INSTALL_DIR) $(redhat_PACKAGE_TOP_BUILDDIR)/SPECS
	$(INSTALL_DIR) $(redhat_PACKAGE_TOP_BUILDDIR)/RPMS/i386
	$(INSTALL_DIR) $(redhat_PACKAGE_TOP_BUILDDIR)/RPMS/$(package_ARCH)
	$(CP) $(redhat_GENERIC_SPEC) $(redhat_SPECIFIC_SPEC)
	$(bindist_LIST) | \
	$(SED) -e 's%^\./%/%' -e '\%^/$$%d' \
		>> $(redhat_SPECIFIC_SPEC)
	$(INSTALL_DATA) $(redhat_SPECIFIC_SPEC) $(redhat_PACKAGE_TOP_BUILDDIR)/SPECS
	$(MAKE) install DESTDIR=$(redhat_PACKAGE_BUILDDIR)
	cd $(redhat_PACKAGE_TOP_BUILDDIR)/SPECS; $(redhat_BUILD) $(redhat_SPEC_FILE)

redhat-clean:
	-$(RM) $(redhat_PACKAGE_BUILDDIR)
])

dnl --------------------------------------------------------------------

dnl page
dnl --------------------------------------------------------------------
dnl End of makefile.
dnl --------------------------------------------------------------------

DS_DEFINE_BLOCK([DS_END],[End of makefile.],[
.PHONY: echo-variable echo-list-variable

# Use this to echo a variable to stdout; example:
# 
#	$ make echo-variable VARIABLE=slack_PACKAGE_NAME
#
echo-variable:
	@echo $($(VARIABLE))

# Use this to  echo a variable to stdout  interpreting it as
# list of strings; example:
# 
#	$ make echo-list-variable VARIABLE=FILES
#
echo-list-variable:
	@$(foreach f,$($(VARIABLE)),echo $(f);)
])


dnl end of file
divert(0)dnl
