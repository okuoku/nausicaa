### Makefile --
#
#

CC		= gcc -std=c99
CWARNINGS	= -Wall
CPPFLAGS	= $(shell curl-config --cflags)
LDFLAGS		= $(shell curl-config --libs)
CFLAGS		= -O2

TARGETS		= get-gna-certs-info get-gna-index

.PHONY: all

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CWARNINGS) $(CPPFLAGS) $(LDFLAGS) $(CFLAGS) -o $(@) $(<)

### end of file
