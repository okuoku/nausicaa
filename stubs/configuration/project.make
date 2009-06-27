# @configure_input@
#

nausicaa_ENABLE_POSIX	= @nausicaa_ENABLE_POSIX@
nausicaa_ENABLE_RANDOM	= @nausicaa_ENABLE_RANDOM@

## --------------------------------------------------------------------

ifeq (yes,$(strip $(nausicaa_ENABLE_POSIX)))

posix_SRCDIR	= $(srcdir)/src/posix
posix_BUILDDIR	= $(builddir)/posix-objects.d
$(eval $(call ds-c-library,posix))

endif

## --------------------------------------------------------------------

ifeq (yes,$(strip $(nausicaa_ENABLE_RANDOM)))

random_SRCDIR	= $(srcdir)/src/random
random_BUILDDIR	= $(builddir)/random-objects.d

$(random_BUILDDIR)/%.@OBJEXT@ : $(random_SRCDIR)/%.h

$(eval $(call ds-c-library,random))

endif

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
