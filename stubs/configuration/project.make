# @configure_input@
#

nausicaa_ENABLE_POSIX	= @nausicaa_ENABLE_POSIX@

ifeq (yes,$(strip $(nausicaa_ENABLE_POSIX)))
posix_PATTERNS	= nausicaa-posix.c
$(eval $(call ds-c-library,posix))
endif

### end of file
# Local Variables:
# mode: makefile-gmake
# End:
